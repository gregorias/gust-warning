-- | This module implements mail sending utilities
module Sendmail (
  Mail (..),
  Sendmail,
  formatSendmail,
  sendmail,
) where

import Config (
  EmailAddress (..),
 )
import Relude
import Turtle (
  select,
  shells,
  textToLines,
 )

-- | A mail message with a title.
data Mail = Mail
  { title :: !Text
  , content :: !Text
  }
  deriving stock (Eq, Show)

-- | A mail message in the format used by Sendmail
newtype Sendmail = Sendmail Text
  deriving newtype (Eq, Show)

formatSendmail :: Mail -> Sendmail
formatSendmail Mail{title = title', content = content'} = Sendmail $ title' <> "\n\n" <> content'

-- | Sends an email using 'sendmail'.
sendmail :: EmailAddress -> Sendmail -> IO ()
sendmail target msg =
  shells
    ("sendmail '" <> coerce target <> "'")
    (select . textToLines . coerce $ msg)
