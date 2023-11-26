-- | A module for representing and parsing the app's configuration.
--
-- I put all data that I don't want to publish on GitHub inside configuration.
module Config (
  EmailAddress (..),
  AppId (..),
  CityId (..),
  WindSpeed (..),
  Config (..),
  parseConfig,
) where

import Relude
import Toml (HasCodec, TomlCodec, decode, genericCodec, prettyTomlDecodeErrors)

newtype AppId = AppId Text
  deriving newtype (Eq, Show)
  deriving stock (Generic)
  deriving newtype (HasCodec)

newtype CityId = CityId Text
  deriving newtype (Eq, Show)
  deriving stock (Generic)
  deriving newtype (HasCodec)

newtype EmailAddress = EmailAddress Text
  deriving newtype (Eq, Show)
  deriving stock (Generic)
  deriving newtype (HasCodec)

newtype WindSpeed = WindSpeed Float
  deriving newtype (Eq, Show)
  deriving stock (Generic)
  deriving newtype (HasCodec)

data Config = Config
  { appId :: !AppId
  , cityId :: !CityId
  , emailAddress :: !EmailAddress
  , windThreshold :: !WindSpeed
  }
  deriving stock (Eq, Show, Generic)

configCodec :: TomlCodec Config
configCodec = genericCodec

parseConfig :: Text -> Either Text Config
parseConfig = first prettyTomlDecodeErrors . decode configCodec
