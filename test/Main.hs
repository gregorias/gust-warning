module Main (main) where

import Data.Aeson (eitherDecode)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Lib (
  TwoAndAHalfForecast (TwoAndAHalfForecast),
  TwoAndAHalfResponse (TwoAndAHalfResponse),
  Wind (Wind),
  isWindy,
 )
import Relude
import Test.Hspec (
  SpecWith,
  describe,
  hspec,
  it,
 )
import Test.Hspec.Expectations.Pretty (shouldBe)
import Text.RawString.QQ (r)

main :: IO ()
main = hspec tests

tests :: SpecWith ()
tests = do
  describe "JSON" $ do
    it "parses a response" $ do
      eitherDecode twoAndAHalfResponse
        `shouldBe` Right
          ( TwoAndAHalfResponse
              "200"
              [ TwoAndAHalfForecast (posixSecondsToUTCTime 1631286000) (Wind 1.37 2.33)
              , TwoAndAHalfForecast (posixSecondsToUTCTime 1631296800) (Wind 1.16 1.09)
              ]
          )
  describe "Wind" $ do
    it "isWindy returns not windy if everything is slower than 10m/s" $ do
      isWindy (Wind 9 9) `shouldBe` False
    it "isWindy returns windy if anything is faster than 10m/s" $ do
      isWindy (Wind 11 9) `shouldBe` True
      isWindy (Wind 9 11) `shouldBe` True

twoAndAHalfResponse :: LByteString
twoAndAHalfResponse =
  [r|
    {
      "cod": "200",
      "message": 0,
      "cnt": 2,
      "list": [
        {
            "dt": 1631286000,
            "main": {
                "temp": 294.87,
                "feels_like": 295,
                "temp_min": 294.23,
                "temp_max": 294.87,
                "pressure": 1016,
                "sea_level": 1016,
                "grnd_level": 969,
                "humidity": 73,
                "temp_kf": 0.64
            },
            "weather": [
                {
                    "id": 500,
                    "main": "Rain",
                    "description": "light rain",
                    "icon": "10d"
                }
            ],
            "clouds": {
                "all": 99
            },
            "wind": {
                "speed": 1.37,
                "deg": 321,
                "gust": 2.33
            },
            "visibility": 9878,
            "pop": 0.65,
            "rain": {
                "3h": 2.1
            },
            "sys": {
                "pod": "d"
            },
            "dt_txt": "2021-09-10 15:00:00"
        },
        {
            "dt": 1631296800,
            "main": {
                "temp": 291.84,
                "feels_like": 291.96,
                "temp_min": 290.16,
                "temp_max": 291.84,
                "pressure": 1017,
                "sea_level": 1017,
                "grnd_level": 969,
                "humidity": 84,
                "temp_kf": 1.68
            },
            "weather": [
                {
                    "id": 500,
                    "main": "Rain",
                    "description": "light rain",
                    "icon": "10n"
                }
            ],
            "clouds": {
                "all": 99
            },
            "wind": {
                "speed": 1.16,
                "deg": 129,
                "gust": 1.09
            },
            "visibility": 10000,
            "pop": 0.68,
            "rain": {
                "3h": 2.19
            },
            "sys": {
                "pod": "n"
            },
            "dt_txt": "2021-09-10 18:00:00"
        }
      ],
      "city": {
          "id": 1234,
          "name": "Xyz",
          "coord": {
              "lat": 1.2345,
              "lon": 1.2345
          },
          "country": "PL",
          "population": 0,
          "timezone": 7200,
          "sunrise": 1631240000,
          "sunset": 1631290000
      }
    }
  |]
