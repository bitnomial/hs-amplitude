{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Amplitude.Track
import Data.Map.Strict qualified as SM
import Data.Text qualified as T
import Data.Time.Clock.POSIX qualified as P
import System.Environment (lookupEnv)
import System.Exit (die)

main :: IO ()
main = do
    -- Get API key from environment
    maybeApiKey <- lookupEnv "AMPLITUDE_API_KEY"
    apiKey <- case maybeApiKey of
        Nothing -> die "Error: AMPLITUDE_API_KEY environment variable not set"
        Just key -> pure $ AmplitudeApiKey (T.pack key)

    -- Create Amplitude client
    client <- createClient apiKey

    -- Track the event
    putStrLn "Sending event to Amplitude..."
    time <- P.getPOSIXTime
    result <- trackEvent client (mkEvent time)

    -- Print the response
    case result of
        Left err -> do
            putStrLn "Error sending event:"
            print err
        Right response -> do
            putStrLn "Success! Response:"
            print response
  where
    eventProp = SM.fromList [("some_event_prop", "some_event_prop_value")]
    userProp = SM.fromList [("some_user_prop", "some_user_prop_value")]
    mkEvent time =
        AmplitudeEvent
            { eventType = "test-event"
            , userId = Just "test_user_123"
            , deviceId = Nothing
            , time = Just time
            , eventProperties = eventProp
            , userProperties = userProp
            , sessionId = Nothing
            }
