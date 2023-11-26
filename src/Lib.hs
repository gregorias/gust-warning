{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Lib (
  TwoAndAHalfResponse (..),
  TwoAndAHalfForecast (..),
  Wind (..),
  isWindy,
  isNextDayWindy,

  -- * The main function
  main,
) where

import Config (
  AppId (AppId),
  CityId (CityId),
  Config (Config),
  EmailAddress (..),
  WindSpeed (..),
  parseConfig,
 )
import Control.Exception (try)
import Data.Aeson (
  FromJSON (parseJSON),
  withObject,
  (.:),
 )
import Data.Text.IO qualified as T
import Data.Time (
  LocalTime,
  UTCTime,
  diffUTCTime,
  getCurrentTime,
  getCurrentTimeZone,
  nominalDay,
  utcToLocalTime,
 )
import Data.Time.Clock (secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Version (showVersion)
import GHC.Generics ()
import Network.HTTP.Req (
  GET (GET),
  HttpException,
  NoReqBody (NoReqBody),
  Option,
  QueryParam (queryParam),
  Scheme (Http),
  Url,
  defaultHttpConfig,
  http,
  jsonResponse,
  req,
  responseBody,
  responseStatusCode,
  responseStatusMessage,
  runReq,
  (/:),
 )
import Optics (
  over,
  _Left,
 )
import Options.Applicative (Parser, execParser, fullDesc, header, help, helper, info, long, metavar, progDesc, simpleVersioner, strOption)
import Paths_gust_warning (version)
import Relude (
  Bool,
  Either (Left),
  Eq ((==)),
  ExceptT,
  FilePath,
  Float,
  Generic,
  IO,
  Maybe (Just, Nothing),
  Monad (return),
  NonEmpty,
  Semigroup ((<>)),
  Show,
  String,
  Text,
  coerce,
  either,
  filter,
  hoistEither,
  lift,
  nonEmpty,
  otherwise,
  putTextLn,
  runExceptT,
  show,
  toText,
  ($),
  (&&),
  (&&&),
  (.),
  (<$>),
  (<**>),
  (<*>),
  (<=),
  (>=),
  (||),
 )
import Relude.List.NonEmpty qualified as NE
import Sendmail (
  Mail (..),
  mail,
 )

data Wind = Wind
  { speed :: Float
  , gust :: Float
  }
  deriving stock (Generic, Show, Eq)

instance FromJSON Wind

data TwoAndAHalfForecast = TwoAndAHalfForecast
  { dt :: UTCTime
  , wind :: Wind
  }
  deriving stock (Generic, Show, Eq)

instance FromJSON TwoAndAHalfForecast where
  parseJSON = withObject "TwoAndAHalfForecast" $ \v ->
    TwoAndAHalfForecast
      <$> (posixSecondsToUTCTime <$> v .: "dt")
      <*> v
      .: "wind"

data TwoAndAHalfResponse = TwoAndAHalfResponse
  { cod :: String
  , list :: [TwoAndAHalfForecast]
  }
  deriving stock (Generic, Show, Eq)

instance FromJSON TwoAndAHalfResponse

-- | Fetches 2.5 day forecast data from Open Weather Map.
fetchForecast :: AppId -> CityId -> ExceptT Text IO TwoAndAHalfResponse
fetchForecast appId cityId = do
  responseOrException <- lift $ try @HttpException runRequest
  httpResponse <- hoistEither . over _Left httpExceptionToText $ responseOrException
  hoistEither $ handleOwmError httpResponse
 where
  url :: Url 'Http
  url = http "api.openweathermap.org" /: "data" /: "2.5" /: "forecast"
  options :: Network.HTTP.Req.Option 'Http
  options =
    queryParam "id" (Just @Text . coerce $ cityId)
      <> queryParam "appid" (Just @Text . coerce $ appId)
  runRequest =
    runReq @IO defaultHttpConfig
      $ req
        GET
        url
        NoReqBody
        (jsonResponse @TwoAndAHalfResponse)
        options
  httpExceptionToText e = "Could not get information from OWM.\n" <> show e
  handleOwmError httpResponse
    | responseStatusCode httpResponse == 200 = return $ responseBody httpResponse
    | otherwise =
        Left
          $ "OWM returned a non-successful status code "
          <> show (responseStatusCode httpResponse)
          <> ".\n"
          <> show (responseStatusMessage httpResponse)
          <> "\n"
          <> show (responseBody httpResponse)

-- | Returns whether the wind is considered "windy."
isWindy :: WindSpeed -> Wind -> Bool
isWindy (WindSpeed threshold) (Wind speed gust) = speed >= threshold || gust >= threshold

-- | Checks whether the next day is expected to be windy.
--
-- Returns expected windy periods if any.
isNextDayWindy :: WindSpeed -> UTCTime -> TwoAndAHalfResponse -> [UTCTime]
isNextDayWindy threshold now (TwoAndAHalfResponse _ forecasts) = dt <$> filter (isWindy threshold . wind) nextDayForecasts
 where
  isWithinNextDay forecast = timeDiff >= secondsToNominalDiffTime 0 && timeDiff <= nominalDay
   where
    timeDiff = diffUTCTime (dt forecast) now
  nextDayForecasts = filter isWithinNextDay forecasts

composeMessage :: NonEmpty LocalTime -> Mail
composeMessage windyTimes =
  Mail
    { title = "Subject: The next 24h will be windy. Secure your NO WIFI picture."
    , content = "The wind will start at " <> start <> " and last till at least " <> end <> "."
    }
 where
  (start, end) = (NE.head &&& NE.last) $ toText . iso8601Show <$> windyTimes

sendWarning :: EmailAddress -> NonEmpty LocalTime -> IO ()
sendWarning target windyPeriods = mail target (composeMessage windyPeriods)

newtype ConfigFilePath = ConfigFilePath FilePath
  deriving newtype (Show, Eq)

configFilePathP :: Parser ConfigFilePath
configFilePathP =
  ConfigFilePath
    <$> strOption
      ( long "config"
          <> metavar "FILE"
          <> help "The TOML config file."
      )

-- | 'main' runs the main program.
--
-- The main program reads provided command-line configuration and does the
-- following:
-- 1. Fetches forecast data.
-- 2. Checks whether the next day will be windy.
-- 3. Sends an e-mail if yes.
main :: IO ()
main = do
  config <- readConfig
  now <- getCurrentTime
  currentTimeZone <- getCurrentTimeZone
  maybeError <- runExceptT $ do
    (Config appId cityId emailAddress windThreshold) <- hoistEither $ parseConfig config
    forecast <- fetchForecast appId cityId
    let windyPeriods = isNextDayWindy windThreshold now forecast
    let utcToLocalTime' = utcToLocalTime currentTimeZone
    case nonEmpty windyPeriods of
      Just someWindyPeriods -> lift $ sendWarning emailAddress (utcToLocalTime' <$> someWindyPeriods)
      Nothing -> return ()
  either
    putTextLn
    return
    maybeError
 where
  opts =
    info
      (configFilePathP <**> helper <**> simpleVersioner (showVersion version))
      ( fullDesc
          <> progDesc "Fetch weather forecast and send a warning if a windy day is coming."
          <> header "gust-warning"
      )
  readConfig = do
    (ConfigFilePath configFilePath) <- execParser opts
    T.readFile configFilePath
