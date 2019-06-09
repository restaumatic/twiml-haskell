{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Text.XML.Twiml.Types
-- Copyright   :  (C) 2018 Mark Andrus Roberts
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Mark Andrus Roberts <markandrusroberts@gmail.com>
-- Stability   :  provisional
-------------------------------------------------------------------------------
module Text.XML.Twiml.Types
  ( Digit(..)
  , Key(..)
  , Method(..)
  , Natural
  , URL
  , parseURL
  , Voice(..)
  , Lang(..)
  , LangAlice(..)
  , VoicePolly(..)
  , voicePollyLanguage
  , Transport(..)
  , ConferenceBeep(..)
  , Reason(..)
  ) where

import Control.DeepSeq (NFData(..))
import Data.Data
import GHC.Generics (Generic)
import Network.URI (URI(..), parseURIReference)

import Text.XML.Twiml.Internal

-- | The ‘digits’ attribute lets you play DTMF tones during a call. See
-- <https://www.twilio.com/docs/api/twiml/play#attributes-digits>.
data Digit
  = D0 -- ^ 0
  | D1 -- ^ 1
  | D2 -- ^ 2
  | D3 -- ^ 3
  | D4 -- ^ 4
  | D5 -- ^ 5
  | D6 -- ^ 6
  | D7 -- ^ 7
  | D8 -- ^ 8
  | D9 -- ^ 9
  | W  -- ^ w
  deriving (Bounded, Data, Enum, Eq, Generic, NFData, Ord, Read, Show, Typeable)

instance ToAttrValue Digit where
  toAttrValue D0 = "0"
  toAttrValue D1 = "1"
  toAttrValue D2 = "2"
  toAttrValue D3 = "3"
  toAttrValue D4 = "4"
  toAttrValue D5 = "5"
  toAttrValue D6 = "6"
  toAttrValue D7 = "7"
  toAttrValue D8 = "8"
  toAttrValue D9 = "9"
  toAttrValue W  = "w"

instance ToAttrValue [Digit] where
  toAttrValue = concatMap toAttrValue

data Key
  = K0      -- ^ 0
  | K1      -- ^ 1
  | K2      -- ^ 2
  | K3      -- ^ 3
  | K4      -- ^ 4
  | K5      -- ^ 5
  | K6      -- ^ 6
  | K7      -- ^ 7
  | K8      -- ^ 8
  | K9      -- ^ 9
  | KStar   -- ^ \*
  | KPound  -- ^ #
  deriving (Bounded, Data, Enum, Eq, Generic, NFData, Ord, Read, Show, Typeable)

instance ToAttrValue Key where
  toAttrValue K0     = "0"
  toAttrValue K1     = "1"
  toAttrValue K2     = "2"
  toAttrValue K3     = "3"
  toAttrValue K4     = "4"
  toAttrValue K5     = "5"
  toAttrValue K6     = "6"
  toAttrValue K7     = "7"
  toAttrValue K8     = "8"
  toAttrValue K9     = "9"
  toAttrValue KStar  = "*"
  toAttrValue KPound = "#"

type Natural = Int

instance ToAttrValue Natural where
  toAttrValue = show

data Method = GET | POST
  deriving (Bounded, Data, Enum, Eq, Generic, NFData, Ord, Read, Show, Typeable)

instance ToAttrValue Method where
  toAttrValue = show

newtype URL = URL { getURL :: String }
  deriving (Data, Eq, Generic, NFData, Ord, Read, Show, Typeable)

instance ToSomeNode URL where
  toSomeNode = toSomeNode . getURL

instance ToAttrValue URL where
  toAttrValue = getURL

-- | Checks whether a @URI@'s scheme, if any, is one of @"http:"@ or @"https:"@.
isHttp :: URI -> Bool
isHttp uri = case uriScheme uri of
  ""       -> True
  "http:"  -> True
  "https:" -> True
  _        -> False

parseURL :: String -> Maybe URL
parseURL url = parseURIReference url
           >>= (\uri -> if isHttp uri then Just (URL url) else Nothing)

-- | Voices supported by @\<Say\>@. See
-- <https://www.twilio.com/docs/api/twiml/say#attributes-voice>.
data Voice
  = Man   (Maybe Lang)
  | Woman (Maybe Lang)
  | Alice (Maybe LangAlice)
  | Polly VoicePolly
  deriving (Data, Eq, Generic, NFData, Ord, Read, Show, Typeable)

-- | Languages spoken by voices 'Man' and 'Woman'. See
-- <https://www.twilio.com/docs/api/twiml/say#attributes-manwoman>.
data Lang
  = English
  | EnglishUK
  | Spanish
  | French
  | German
  | Italian
  deriving (Data, Eq, Generic, NFData, Ord, Read, Show, Typeable)

instance ToAttrValue Lang where
  toAttrValue English   = "en"
  toAttrValue EnglishUK = "en-gb"
  toAttrValue Spanish   = "es"
  toAttrValue French    = "fr"
  toAttrValue German    = "de"
  toAttrValue Italian   = "it"

-- | Languages spoken by 'Alice'. See
-- <https://www.twilio.com/docs/api/twiml/say#attributes-alice>.
data LangAlice
  = DaDK -- ^ Danish, Denmark
  | DeDE -- ^ German, Germany
  | EnAU -- ^ English, Australia
  | EnCA -- ^ English, Canada
  | EnGB -- ^ English, UK
  | EnIN -- ^ English, India
  | EnUS -- ^ English, United States
  | CaES -- ^ Catalan, Spain
  | EsES -- ^ Spanish, Spain
  | EsMX -- ^ Spanish, Mexico
  | FiFI -- ^ Finnish, Finland
  | FrCA -- ^ French, Canada
  | FrFR -- ^ French, France
  | ItIT -- ^ Italian, Italy
  | JaJP -- ^ Japanese, Japan
  | KoKR -- ^ Korean, Korea
  | NbNO -- ^ Norwegian, Norway
  | NlNL -- ^ Dutch, Netherlands
  | PlPL -- ^ Polish-Poland
  | PtBR -- ^ Portuguese, Brazil
  | PtPT -- ^ Portuguese, Portugal
  | RuRU -- ^ Russian, Russia
  | SvSE -- ^ Swedish, Sweden
  | ZhCN -- ^ Chinese (Mandarin)
  | ZhHK -- ^ Chinese (Cantonese)
  | ZhTW -- ^ Chinese (Taiwanese Mandarin)
  deriving (Data, Eq, Generic, NFData, Ord, Read, Show, Typeable)

instance ToAttrValue LangAlice where
  toAttrValue DaDK = "da-DK"
  toAttrValue DeDE = "de-DE"
  toAttrValue EnAU = "en-AU"
  toAttrValue EnCA = "en-CA"
  toAttrValue EnGB = "en-GB"
  toAttrValue EnIN = "en-IN"
  toAttrValue EnUS = "en-US"
  toAttrValue CaES = "ca-ES"
  toAttrValue EsES = "es-ES"
  toAttrValue EsMX = "es-MX"
  toAttrValue FiFI = "fi-FI"
  toAttrValue FrCA = "fr-CA"
  toAttrValue FrFR = "fr-FR"
  toAttrValue ItIT = "it-IT"
  toAttrValue JaJP = "ja-JP"
  toAttrValue KoKR = "ko-KR"
  toAttrValue NbNO = "nb-NO"
  toAttrValue NlNL = "nl-NL"
  toAttrValue PlPL = "pl-PL"
  toAttrValue PtBR = "pt-BR"
  toAttrValue PtPT = "pt-PT"
  toAttrValue RuRU = "ru-RU"
  toAttrValue SvSE = "sv-SE"
  toAttrValue ZhCN = "zh-CN"
  toAttrValue ZhHK = "zh-HK"
  toAttrValue ZhTW = "zh-TW"

-- | Voices available when using the Polly text-to-speech engine. See
-- <https://www.twilio.com/docs/voice/twiml/say/text-speech#voices>.
data VoicePolly
  = PollyMads            -- Danish (da-DK), Male
  | PollyNaja            -- Danish (da-DK), Female
  | PollyLotte           -- Dutch (nl-NL), Female
  | PollyRuben           -- Dutch (nl-NL), Male
  | PollyNicole          -- English (Australian) (en-AU), Female
  | PollyRussell         -- English (Australian) (en-AU), Male
  | PollyAmy             -- English (British) (en-GB), Female
  | PollyBrian           -- English (British) (en-GB), Male
  | PollyEmma            -- English (British) (en-GB), Female
  | PollyRaveena         -- English (Indian) (en-IN), Female
  | PollyIvy             -- English (US) (en-US), Female
  | PollyJoanna          -- English (US) (en-US), Female
  | PollyJoey            -- English (US) (en-US), Male
  | PollyJustin          -- English (US) (en-US), Male
  | PollyKendra          -- English (US) (en-US), Female
  | PollyKimberly        -- English (US) (en-US), Female
  | PollyMatthew         -- English (US) (en-US), Male
  | PollySalli           -- English (US) (en-US), Female
  | PollyGeraint         -- English (Welsh) (en-GB-WLS), Male
  | PollyCeline          -- French (fr-FR), Female
  | PollyMathieu         -- French (fr-FR), Male
  | PollyChantal         -- French (Canadian) (fr-CA), Female
  | PollyHans            -- German (de-DE), Male
  | PollyMarlene         -- German (de-DE), Female
  | PollyVicki           -- German (de-DE), Female
  | PollyDora            -- Icelandic (is-IS), Female
  | PollyKarl            -- Icelandic (is-IS), Male
  | PollyCarla           -- Italian (it-IT), Female
  | PollyGiorgio         -- Italian (it-IT), Male
  | PollyMizuki          -- Japanese (ja-JP), Female
  | PollyTakumi          -- Japanese (ja-JP), Male
  | PollyLiv             -- Norwegian (nb-NO), Female
  | PollyJacek           -- Polish (pl-PL), Male
  | PollyJan             -- Polish (pl-PL), Male
  | PollyEwa             -- Polish (pl-PL), Female
  | PollyMaja            -- Polish (pl-PL), Female
  | PollyRicardo         -- Portuguese (Brazilian) (pt-BR), Male
  | PollyVitoria         -- Portuguese (Brazilian) (pt-BR), Female
  | PollyCristiano       -- Portuguese (European) (pt-PT), Male
  | PollyInes            -- Portuguese (European) (pt-PT), Female
  | PollyCarmen          -- Romanian (ro-RO), Female
  | PollyMaxim           -- Russian (ru-RU), Male
  | PollyTatyana         -- Russian (ru-RU), Female
  | PollyConchita        -- Spanish (Castilian) (es-ES), Female
  | PollyEnrique         -- Spanish (Castilian) (es-ES), Male
  | PollyMiguel          -- Spanish (Latin American) (es-US), Male
  | PollyPenelope        -- Spanish (Latin American) (es-US), Female
  | PollyAstrid          -- Swedish (sv-SE), Female
  | PollyFiliz           -- Turkish (tr-TR), Female
  | PollyGwyneth         -- Welsh (cy-GB), Female
  deriving (Data, Eq, Generic, NFData, Ord, Read, Show, Typeable)

instance ToAttrValue VoicePolly where
  toAttrValue PollyMads            = "Polly.Mads"
  toAttrValue PollyNaja            = "Polly.Naja"
  toAttrValue PollyLotte           = "Polly.Lotte"
  toAttrValue PollyRuben           = "Polly.Ruben"
  toAttrValue PollyNicole          = "Polly.Nicole"
  toAttrValue PollyRussell         = "Polly.Russell"
  toAttrValue PollyAmy             = "Polly.Amy"
  toAttrValue PollyBrian           = "Polly.Brian"
  toAttrValue PollyEmma            = "Polly.Emma"
  toAttrValue PollyRaveena         = "Polly.Raveena"
  toAttrValue PollyIvy             = "Polly.Ivy"
  toAttrValue PollyJoanna          = "Polly.Joanna"
  toAttrValue PollyJoey            = "Polly.Joey"
  toAttrValue PollyJustin          = "Polly.Justin"
  toAttrValue PollyKendra          = "Polly.Kendra"
  toAttrValue PollyKimberly        = "Polly.Kimberly"
  toAttrValue PollyMatthew         = "Polly.Matthew"
  toAttrValue PollySalli           = "Polly.Salli"
  toAttrValue PollyGeraint         = "Polly.Geraint"
  toAttrValue PollyCeline          = "Polly.Celine"
  toAttrValue PollyMathieu         = "Polly.Mathieu"
  toAttrValue PollyChantal         = "Polly.Chantal"
  toAttrValue PollyHans            = "Polly.Hans"
  toAttrValue PollyMarlene         = "Polly.Marlene"
  toAttrValue PollyVicki           = "Polly.Vicki"
  toAttrValue PollyDora            = "Polly.Dora"
  toAttrValue PollyKarl            = "Polly.Karl"
  toAttrValue PollyCarla           = "Polly.Carla"
  toAttrValue PollyGiorgio         = "Polly.Giorgio"
  toAttrValue PollyMizuki          = "Polly.Mizuki"
  toAttrValue PollyTakumi          = "Polly.Takumi"
  toAttrValue PollyLiv             = "Polly.Liv"
  toAttrValue PollyJacek           = "Polly.Jacek"
  toAttrValue PollyJan             = "Polly.Jan"
  toAttrValue PollyEwa             = "Polly.Ewa"
  toAttrValue PollyMaja            = "Polly.Maja"
  toAttrValue PollyRicardo         = "Polly.Ricardo"
  toAttrValue PollyVitoria         = "Polly.Vitoria"
  toAttrValue PollyCristiano       = "Polly.Cristiano"
  toAttrValue PollyInes            = "Polly.Ines"
  toAttrValue PollyCarmen          = "Polly.Carmen"
  toAttrValue PollyMaxim           = "Polly.Maxim"
  toAttrValue PollyTatyana         = "Polly.Tatyana"
  toAttrValue PollyConchita        = "Polly.Conchita"
  toAttrValue PollyEnrique         = "Polly.Enrique"
  toAttrValue PollyMiguel          = "Polly.Miguel"
  toAttrValue PollyPenelope        = "Polly.Penelope"
  toAttrValue PollyAstrid          = "Polly.Astrid"
  toAttrValue PollyFiliz           = "Polly.Filiz"
  toAttrValue PollyGwyneth         = "Polly.Gwyneth"

voicePollyLanguage :: VoicePolly -> String
voicePollyLanguage PollyMads            = "da-DK"
voicePollyLanguage PollyNaja            = "da-DK"
voicePollyLanguage PollyLotte           = "nl-NL"
voicePollyLanguage PollyRuben           = "nl-NL"
voicePollyLanguage PollyNicole          = "en-AU"
voicePollyLanguage PollyRussell         = "en-AU"
voicePollyLanguage PollyAmy             = "en-GB"
voicePollyLanguage PollyBrian           = "en-GB"
voicePollyLanguage PollyEmma            = "en-GB"
voicePollyLanguage PollyRaveena         = "en-IN"
voicePollyLanguage PollyIvy             = "en-US"
voicePollyLanguage PollyJoanna          = "en-US"
voicePollyLanguage PollyJoey            = "en-US"
voicePollyLanguage PollyJustin          = "en-US"
voicePollyLanguage PollyKendra          = "en-US"
voicePollyLanguage PollyKimberly        = "en-US"
voicePollyLanguage PollyMatthew         = "en-US"
voicePollyLanguage PollySalli           = "en-US"
voicePollyLanguage PollyGeraint         = "en-GB-WLS"
voicePollyLanguage PollyCeline          = "fr-FR"
voicePollyLanguage PollyMathieu         = "fr-FR"
voicePollyLanguage PollyChantal         = "fr-CA"
voicePollyLanguage PollyHans            = "de-DE"
voicePollyLanguage PollyMarlene         = "de-DE"
voicePollyLanguage PollyVicki           = "de-DE"
voicePollyLanguage PollyDora            = "is-IS"
voicePollyLanguage PollyKarl            = "is-IS"
voicePollyLanguage PollyCarla           = "it-IT"
voicePollyLanguage PollyGiorgio         = "it-IT"
voicePollyLanguage PollyMizuki          = "ja-JP"
voicePollyLanguage PollyTakumi          = "ja-JP"
voicePollyLanguage PollyLiv             = "nb-NO"
voicePollyLanguage PollyJacek           = "pl-PL"
voicePollyLanguage PollyJan             = "pl-PL"
voicePollyLanguage PollyEwa             = "pl-PL"
voicePollyLanguage PollyMaja            = "pl-PL"
voicePollyLanguage PollyRicardo         = "pt-BR"
voicePollyLanguage PollyVitoria         = "pt-BR"
voicePollyLanguage PollyCristiano       = "pt-PT"
voicePollyLanguage PollyInes            = "pt-PT"
voicePollyLanguage PollyCarmen          = "ro-RO"
voicePollyLanguage PollyMaxim           = "ru-RU"
voicePollyLanguage PollyTatyana         = "ru-RU"
voicePollyLanguage PollyConchita        = "es-ES"
voicePollyLanguage PollyEnrique         = "es-ES"
voicePollyLanguage PollyMiguel          = "es-US"
voicePollyLanguage PollyPenelope        = "es-US"
voicePollyLanguage PollyAstrid          = "sv-SE"
voicePollyLanguage PollyFiliz           = "tr-TR"
voicePollyLanguage PollyGwyneth         = "cy-GB"

-- | See <https://www.twilio.com/docs/api/twiml/sip#transport>.
data Transport = TCP | UDP
  deriving (Bounded, Data, Enum, Eq, Generic, NFData, Ord, Read, Show, Typeable)

instance ToAttrValue Transport where
  toAttrValue TCP = "tcp"
  toAttrValue UDP = "udp"

-- | See <https://www.twilio.com/docs/api/twiml/conference#attributes-beep>.
data ConferenceBeep
  = Yes
  | No
  | OnExit
  | OnEnter
  deriving (Bounded, Data, Enum, Eq, Generic, NFData, Ord, Read, Show, Typeable)

instance ToAttrValue ConferenceBeep where
  toAttrValue Yes     = "yes"
  toAttrValue No      = "no"
  toAttrValue OnExit  = "on-exit"
  toAttrValue OnEnter = "on-enter"

-- | The reason attribute takes the values \"rejected\" and \"busy.\" This tells
-- Twilio what message to play when rejecting a call. Selecting \"busy\" will
-- play a busy signal to the caller, while selecting \"rejected\" will play a
-- standard not-in-service response.
-- See <https://www.twilio.com/docs/api/twiml/reject#attributes-reason>.
data Reason = Rejected | Busy
  deriving (Bounded, Data, Enum, Eq, Generic, NFData, Ord, Read, Show, Typeable)

instance ToAttrValue Reason where
  toAttrValue Rejected = "rejected"
  toAttrValue Busy     = "busy"

instance ToAttrValue Voice where
  toAttrValue (Man   _) = "man"
  toAttrValue (Woman _) = "woman"
  toAttrValue (Alice _) = "alice"
  toAttrValue (Polly v) = toAttrValue v
