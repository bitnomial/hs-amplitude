{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Amplitude.Track (
    -- * Types
    AmplitudeClient (..),
    AmplitudeHttpV2Request (..),
    AmplitudeEvent (..),
    AmplitudeResponse (..),

    -- * Smart constructors
    mkEvent,
    mkEventWithUserId,
    mkEventWithDeviceId,
) where

import Data.Aeson (
    FromJSON,
    ToJSON,
    object,
    withObject,
    (.:), -- (.:?),
    (.=),
 )
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API (
    BasicAuth,
    BasicAuthData (..),
    Capture,
    JSON,
    NoContent (..),
    Post,
    ReqBody,
    (:>),
 )
import Servant.Client (
    ClientEnv,
    ClientError,
    ClientM,
    client,
    mkClientEnv,
    parseBaseUrl,
    runClientM,
 )

data AmplitudeClient = AmplitudeClient
    { apiKey :: AmplitudeApiKey
    , clientEnv :: ClientEnv
    }

newtype AmplitudeApiKey = AmplitudeApiKey Text

createClient :: AmplitudeApiKey -> IO AmplitudeClient
createClient = do
    manager <- newTlsManager
    let baseUrl = fromMaybe (error "Invalid hardcoded URL") $ parseBaseUrl "https://track.customer.io"
        env = mkClientEnv manager baseUrl
    pure $ AmplitudeClient auth env

-- | Request body for Amplitude HTTP API V2
data AmplitudeHttpV2Request = AmplitudeHttpV2Request
    { apiKey :: Text
    , events :: [AmplitudeEvent]
    }
    deriving (Show, Generic)

instance ToJSON AmplitudeHttpV2Request where
    toJSON req =
        object
            [ "api_key" .= req.apiKey
            , "events" .= req.events
            ]

type HttpV2API =
    "2"
        :> "httpapi"
        :> ReqBody '[JSON] AmplitudeHttpV2Request
        :> Post '[JSON] NoContent

{- | An Amplitude event
Requires: event_type and at least one of user_id or device_id
-}
data AmplitudeEvent = AmplitudeEvent
    { eventType :: Text
    , userId :: Maybe Text
    , deviceId :: Maybe Text
    , time :: Maybe Integer -- milliseconds since epoch
    , eventProperties :: Maybe Aeson.Value
    , userProperties :: Maybe Aeson.Value
    , sessionId :: Maybe Integer
    , insertId :: Maybe Text
    }
    deriving (Show, Generic)

instance ToJSON AmplitudeEvent where
    toJSON evt =
        object $
            filter
                (notNull . snd)
                [ "event_type" .= eventType evt
                , "user_id" .= userId evt
                , "device_id" .= deviceId evt
                , "time" .= time evt
                , "event_properties" .= eventProperties evt
                , "user_properties" .= userProperties evt
                , "session_id" .= sessionId evt
                , "insert_id" .= insertId evt
                ]
      where
        notNull Aeson.Null = False
        notNull _ = True

-- | Response from Amplitude HTTP API V2
data AmplitudeResponse = AmplitudeResponse
    { code :: Int
    , eventsIngested :: Int
    , payloadSizeBytes :: Int
    , serverUploadTime :: Integer
    }
    deriving (Show, Generic)

instance FromJSON AmplitudeResponse where
    parseJSON = withObject "AmplitudeResponse" $ \v ->
        AmplitudeResponse
            <$> v .: "code"
            <*> v .: "events_ingested"
            <*> v .: "payload_size_bytes"
            <*> v .: "server_upload_time"

-- | Create an event with a user ID
mkEventWithUserId :: Text -> Text -> AmplitudeEvent
mkEventWithUserId eventName uid =
    AmplitudeEvent
        { eventType = eventName
        , userId = Just uid
        , deviceId = Nothing
        , time = Nothing
        , eventProperties = Nothing
        , userProperties = Nothing
        , sessionId = Nothing
        , insertId = Nothing
        }

-- | Create an event with a device ID
mkEventWithDeviceId :: Text -> Text -> AmplitudeEvent
mkEventWithDeviceId eventName did =
    AmplitudeEvent
        { eventType = eventName
        , userId = Nothing
        , deviceId = Just did
        , time = Nothing
        , eventProperties = Nothing
        , userProperties = Nothing
        , sessionId = Nothing
        , insertId = Nothing
        }

-- | Create an event with both user ID and device ID
mkEvent :: Text -> Text -> Text -> AmplitudeEvent
mkEvent eventName uid did =
    AmplitudeEvent
        { eventType = eventName
        , userId = Just uid
        , deviceId = Just did
        , time = Nothing
        , eventProperties = Nothing
        , userProperties = Nothing
        , sessionId = Nothing
        , insertId = Nothing
        }
