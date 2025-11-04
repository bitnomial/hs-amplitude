{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Amplitude.Track
import Data.Map.Strict qualified as SM
import Data.Text qualified as T
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

    -- Create a sample event
    let event = mkEventWithUserId "test_event" "test_user_123" eventProp userProp

    -- Track the event
    putStrLn "Sending event to Amplitude..."
    result <- trackEvent client event

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
