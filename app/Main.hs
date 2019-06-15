{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

import           Control.Concurrent.Lifted            (fork, threadDelay)
import           Control.Exception                    hiding (Handler)
import           Control.Monad                        (forM, forM_, forever)
import           Control.Monad.Error.Class
import           Control.Monad.Except
import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Control.Monad.Reader
import           Control.Monad.Trans.Class            (lift)
import           Control.Monad.Trans.Control          (MonadBaseControl,
                                                       restoreT)
import           Control.Monad.Trans.Maybe            (runMaybeT)
import           Data.Aeson
import           Data.Aeson.QQ                        (aesonQQ)
import           Data.Aeson.Types
import           Data.Bifunctor
import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Lazy                 as LBS
import           Data.List.Extra                      (minimumOn)
import           Data.Maybe                           (catMaybes)
import           Data.String                          (fromString)
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import qualified Data.Vector                          as V
import           Database.PostgreSQL.Simple           as P hiding ((:.))
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.SqlQQ
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.FromRow
import           Line.Bot.Client
import           Line.Bot.Types                       as B
import           Line.Bot.Webhook                     as W
import           Network.Connection
import           Network.HTTP.Conduit                 hiding (Proxy)
import           Network.HTTP.Simple                  hiding (Proxy)
import           Network.Wai.Handler.Warp             (run)
import           Servant
import           Servant.Client
import           Servant.Server                       (Context ((:.), EmptyContext))
import           System.Environment                   (getEnv)
import           Text.Read                            (readMaybe)

data AQData = AQData
    { aqi    :: Int
    , county :: Text
    , lat    :: Double
    , lng    :: Double
    , status :: Text
    , pm25   :: Int
    , pm10   :: Int
    , o3     :: Int
    , co     :: Double
    , so2    :: Double
    }
  deriving (Eq, Show)

data Env = Env
  { envToken  :: ChannelToken
  , envSecret :: ChannelSecret
  , envConn   :: P.Connection
  }

class Monad m => MonadChannel m where
  getToken  :: m ChannelToken
  getSecret :: m ChannelSecret

instance Monad m => MonadChannel (ReaderT Env m) where
  getToken  = ask >>= return . envToken
  getSecret = ask >>= return . envSecret

instance FromField Source where
  fromField = fromJSONField

instance FromRow Source where
  fromRow = field

instance ToField Source where
  toField = toJSONField

class Monad m => MonadDatabase m where
  getUsers :: Coord -> m [Source]
  addUser  :: Coord -> Source -> m ()

instance MonadIO m => MonadDatabase (ReaderT Env m) where
  getUsers (lat, lng) = do
    Env{envConn} <- ask
    liftIO $ query envConn q (lng, lat, 30000 :: Double)
    where
      q = [sql|
        SELECT source
          FROM users
         WHERE ST_DWithin(location, ST_Point(?, ?)::geography, ?)
      |]

  addUser (lat, lng) src = do
    Env{envConn} <- ask
    _ <- liftIO $ execute envConn q (src, lng, lat)
    return ()
    where
      q = [sql|
        INSERT INTO users(source, location)
             VALUES (?, ST_SetSRID(ST_MakePoint(?, ?), 4326))
      |]

parseAQData :: Value -> Parser (Maybe AQData)
parseAQData = withObject "AQData" $ \o -> runMaybeT $ do
  aqi    <- restoreT $ readMaybe <$> o .: "AQI"
  county <- lift     $               o .: "County"
  lat    <- restoreT $ readMaybe <$> o .: "Latitude"
  lng    <- restoreT $ readMaybe <$> o .: "Longitude"
  status <- lift     $               o .: "Status"
  pm25   <- restoreT $ readMaybe <$> o .: "PM2.5"
  pm10   <- restoreT $ readMaybe <$> o .: "PM10"
  o3     <- restoreT $ readMaybe <$> o .: "O3"
  co     <- restoreT $ readMaybe <$> o .: "CO"
  so2    <- restoreT $ readMaybe <$> o .: "SO2"
  return AQData {..}

instance FromJSON AQData where
  parseJSONList = withArray "[AQData]" $ \arr ->
    catMaybes <$> forM (V.toList arr) parseAQData

  parseJSON _   = fail "not an array"

noSSLVerifyManager :: IO Manager
noSSLVerifyManager =
  let tlsSettings = TLSSettingsSimple
        { settingDisableCertificateValidation = True
        , settingDisableSession               = False
        , settingUseServerName                = True
        }
  in newManager $ mkManagerSettings tlsSettings Nothing

getAQData :: IO [AQData]
getAQData = do
  manager <- noSSLVerifyManager
  eresponse <- try $ httpJSON $ setRequestManager manager opendata
  case eresponse of
    Left e -> do
      print (e :: HttpException)
      getAQData -- retry
    Right response -> do
      let body = getResponseBody response
      return body
  where
    opendata = "https://opendata.epa.gov.tw/ws/Data/AQI?$format=json"

type Coord = (Double, Double)

distRad :: Double -> Coord -> Coord -> Double
distRad radius (lat1, lng1) (lat2, lng2) = 2 * radius * asin (min 1.0 root)
  where
    hlat = hsin (lat2 - lat1)
    hlng = hsin (lng2 - lng1)
    root = sqrt (hlat + cos lat1 * cos lat2 * hlng)
    hsin = (^ 2) . sin . (/ 2) -- harvesine of an angle

distDeg :: Double -> Coord -> Coord -> Double
distDeg radius p1 p2 = distRad radius (deg2rad p1) (deg2rad p2)
  where
    d2r = (/ 180) . (* pi)
    deg2rad = bimap d2r d2r

distance :: Coord -> Coord -> Double
distance = distDeg 6371 --earth radius

getCoord :: AQData -> Coord
getCoord AQData{..} = (lat, lng)

closestTo :: [AQData] -> Coord -> AQData
closestTo xs coord = (distance coord . getCoord) `minimumOn` xs

askLoc :: (MonadIO m, MonadChannel m) => ReplyToken -> m ()
askLoc rt = do
  token <- getToken
  _ <- liftIO $ runLine comp token
  return ()
    where
      welcome = "Where are you?"
      qr      = QuickReply [QuickReplyButton Nothing (ActionLocation "location")]
      comp    = replyMessage rt [B.MessageText welcome (Just qr)]

unhealthy :: AQData -> Bool
unhealthy AQData{..} = aqi > 100

notifyChat :: (Source, AQData) -> Line NoContent
notifyChat (Source a, x) = pushMessage a [mkMessage x]

loop
  :: ( MonadChannel m
     , MonadDatabase m
     , MonadIO m
     , MonadBaseControl IO m
     )
  => m ()
loop = do
  fork $ forever $ do
    token <- getToken
    as <- liftIO $ filter unhealthy <$> getAQData
    forM_ as $ \a -> do
      let coord = (lat a, lng a)
      users <- getUsers coord
      liftIO $ forM_ users $ \src -> runLine (notifyChat (src, a)) token
    threadDelay (3600 * 10^6)
  return ()

webhook
  :: ( MonadChannel m
     , MonadDatabase m
     , MonadIO m
     )
  => [Event]
  -> m NoContent
webhook events = do
  forM_ events $ \case
    EventFollow  {..} -> askLoc replyToken
    EventJoin    {..} -> askLoc replyToken
    EventMessage { message = W.MessageLocation {..}
                 , source
                 }    -> addUser (latitude, longitude) source
  return NoContent

aqServer :: ServerT Webhook (ReaderT Env Handler)
aqServer = webhook . events

api = Proxy :: Proxy Webhook
pc  = Proxy :: Proxy '[ChannelSecret]

app :: (MonadReader Env m) => m Application
app = do
  env <- ask
  let server = hoistServerWithContext api pc (`runReaderT` env) aqServer
  return $ serveWithContext api (envSecret env :. EmptyContext) server

main :: IO ()
main = do
  token  <- fromString <$> getEnv "CHANNEL_TOKEN"
  secret <- fromString <$> getEnv "CHANNEL_SECRET"
  conn   <- connect defaultConnectInfo
  let env = Env token secret conn
  runReaderT loop env
  run 3000 $ runReader app env

mkMessage :: AQData -> B.Message
mkMessage x = B.MessageFlex "air quality alert!" (flexContent x) Nothing

flexContent :: AQData -> Value
flexContent AQData{..} = [aesonQQ|
  {
    "type": "bubble",
    "styles": {
      "footer": {
        "separator": true
      }
    },
    "header": {
      "type": "box",
      "layout": "vertical",
      "contents": [
        {
          "type": "text",
          "text": "AIR QUALITY ALERT",
          "weight": "bold",
          "size": "xl",
          "color": "#ff0000",
          "margin": "md"
        },
        {
          "type": "text",
          "text": "Unhealthy air reported in your area",
          "size": "xs",
          "color": "#aaaaaa",
          "wrap": true
        }
      ]
    },
    "body": {
      "type": "box",
      "layout": "vertical",
      "contents": [
        {
          "type": "box",
          "layout": "vertical",
          "margin": "xxl",
          "spacing": "sm",
          "contents": [
            {
              "type": "box",
              "layout": "horizontal",
              "contents": [
                {
                  "type": "text",
                  "text": "County",
                  "size": "sm",
                  "color": "#555555",
                  "flex": 0
                },
                {
                  "type": "text",
                  "text": #{county},
                  "size": "sm",
                  "color": "#111111",
                  "align": "end"
                }
              ]
            },
            {
              "type": "box",
              "layout": "horizontal",
              "contents": [
                {
                  "type": "text",
                  "text": "Status",
                  "size": "sm",
                  "color": "#555555",
                  "flex": 0
                },
                {
                  "type": "text",
                  "text": #{status},
                  "size": "sm",
                  "color": "#111111",
                  "align": "end"
                }
              ]
            },
            {
              "type": "box",
              "layout": "horizontal",
              "contents": [
                {
                  "type": "text",
                  "text": "AQI",
                  "weight": "bold",
                  "size": "sm",
                  "color": "#ff0000",
                  "flex": 0
                },
                {
                  "type": "text",
                  "text": #{show aqi},
                  "weight": "bold",
                  "size": "sm",
                  "color": "#ff0000",
                  "align": "end"
                }
              ]
            },
            {
              "type": "box",
              "layout": "horizontal",
              "contents": [
                {
                  "type": "text",
                  "text": "PM2.5",
                  "size": "sm",
                  "color": "#555555",
                  "flex": 0
                },
                {
                  "type": "text",
                  "text": #{show pm25},
                  "size": "sm",
                  "color": "#111111",
                  "align": "end"
                }
              ]
            },
            {
              "type": "box",
              "layout": "horizontal",
              "contents": [
                {
                  "type": "text",
                  "text": "PM10",
                  "size": "sm",
                  "color": "#555555",
                  "flex": 0
                },
                {
                  "type": "text",
                  "text": #{show pm10},
                  "size": "sm",
                  "color": "#111111",
                  "align": "end"
                }
              ]
            },
            {
              "type": "box",
              "layout": "horizontal",
              "contents": [
                {
                  "type": "text",
                  "text": "O3",
                  "size": "sm",
                  "color": "#555555",
                  "flex": 0
                },
                {
                  "type": "text",
                  "text": #{show o3},
                  "size": "sm",
                  "color": "#111111",
                  "align": "end"
                }
              ]
            },
            {
              "type": "box",
              "layout": "horizontal",
              "contents": [
                {
                  "type": "text",
                  "text": "CO",
                  "size": "sm",
                  "color": "#555555",
                  "flex": 0
                },
                {
                  "type": "text",
                  "text": #{show co},
                  "size": "sm",
                  "color": "#111111",
                  "align": "end"
                }
              ]
            },
            {
              "type": "box",
              "layout": "horizontal",
              "contents": [
                {
                  "type": "text",
                  "text": "SO2",
                  "size": "sm",
                  "color": "#555555",
                  "flex": 0
                },
                {
                  "type": "text",
                  "text": #{show so2},
                  "size": "sm",
                  "color": "#111111",
                  "align": "end"
                }
              ]
            }
          ]
        }
      ]
    },
    "footer": {
      "type": "box",
      "layout": "horizontal",
      "contents": [
        {
          "type": "button",
          "action": {
            "type": "uri",
            "label": "More info",
            "uri": "https://www.epa.gov.tw/"
          }
        }
      ]
    }
  }
|]
