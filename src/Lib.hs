{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Lib (
  TwoAndAHalfResponse (..),
  TwoAndAHalfForecast (..),
  Wind (..),
  isWindy,
  isNextDayWindy,
  main,
) where

import Config (AppId (AppId), CityId (CityId), Config (Config), EmailAddress (..), parseConfig)
import Control.Exception (try)
import Data.Aeson (
  FromJSON (parseJSON),
  withObject,
  (.:),
 )
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
import Options.Applicative (Parser, execParser, fullDesc, header, help, helper, info, long, metavar, progDesc, strOption)
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
  readFileText,
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
import qualified Relude.List.NonEmpty as NE
import Turtle (
  select,
  shells,
  textToLines,
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
      <*> v .: "wind"

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
    runReq @IO defaultHttpConfig $
      req
        GET
        url
        NoReqBody
        (jsonResponse @TwoAndAHalfResponse)
        options
  httpExceptionToText e = "Could not get information from OWM.\n" <> show e
  handleOwmError httpResponse
    | responseStatusCode httpResponse == 200 = return $ responseBody httpResponse
    | otherwise =
      Left $
        "OWM returned a non-successful status code "
          <> show (responseStatusCode httpResponse)
          <> ".\n"
          <> show (responseStatusMessage httpResponse)
          <> "\n"
          <> show (responseBody httpResponse)

-- | Returns whether the wind is considered "windy."
--
-- I chose the 6 Beaufort number, "Large branches in motion; whistling heard in
-- telegraph wires; umbrellas used with difficulty." This sounds like windy
-- enough to move the picture I have.
isWindy :: Wind -> Bool
isWindy (Wind speed gust) = speed >= 10 || gust >= 10

-- | Checks whether the next day is expected to be windy.
--
-- Returns expected windy periods if any.
isNextDayWindy :: UTCTime -> TwoAndAHalfResponse -> [UTCTime]
isNextDayWindy now (TwoAndAHalfResponse _ forecasts) = dt <$> filter (isWindy . wind) nextDayForecasts
 where
  isWithinNextDay forecast = timeDiff >= secondsToNominalDiffTime 0 && timeDiff <= nominalDay
   where
    timeDiff = diffUTCTime (dt forecast) now
  nextDayForecasts = filter isWithinNextDay forecasts

-- | A mail message with a title.
data Mail = Mail
  { title :: !Text
  , content :: !Text
  }
  deriving stock (Eq, Show)

composeMessage :: NonEmpty LocalTime -> Mail
composeMessage windyTimes =
  Mail
    { title = "Subject: The next 24h will be windy. Secure your NO WIFI picture."
    , content = "The wind will start at " <> start <> " and last till at least " <> end <> "."
    }
 where
  (start, end) = (NE.head &&& NE.last) $ toText . iso8601Show <$> windyTimes

-- | A mail message in the format used by Sendmail
newtype Sendmail = Sendmail Text
  deriving newtype (Eq, Show)

formatMail :: Mail -> Sendmail
formatMail Mail{title = title, content = content} = Sendmail $ title <> "\n\n" <> content

sendWarning :: EmailAddress -> NonEmpty LocalTime -> IO ()
sendWarning target windyPeriods =
  shells
    ("sendmail '" <> coerce target <> "'")
    (select . textToLines . coerce . formatMail . composeMessage $ windyPeriods)

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

main :: IO ()
main = do
  (ConfigFilePath configFilePath) <- execParser opts
  config <- readFileText configFilePath
  now <- getCurrentTime
  currentTimeZone <- getCurrentTimeZone
  maybeError <- runExceptT $ do
    (Config appId cityId emailAddress) <- hoistEither $ parseConfig config
    forecast <- fetchForecast appId cityId
    let windyPeriods = isNextDayWindy now forecast
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
      (configFilePathP <**> helper)
      ( fullDesc
          <> progDesc "Fetch weather forecast and send a warning if a windy day is coming."
          <> header "gust-warning"
      )