{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import           Network.HTTP.Client      (newManager)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Web.Telegram.API.Bot

main :: IO ()
main = getName

getName :: IO()
getName = do
  manager <- newManager tlsManagerSettings
  res <- getMe token manager
  case res of
    Left e -> do
      putStrLn "Request failed"
      print e
    Right Response { result = u } -> do
      putStrLn "Request succeded"
      print $ user_id u
  where token = Token "bot788362389:AAEGzjN2HXWE-fwF_-m56lYhfGfMMZYj9-0"

sendM :: IO()
sendM = do
  manager <- newManager tlsManagerSettings
  let request = sendMessageRequest chatId message
  res <- sendMessage token request manager
  case res of
    Left e -> do
      putStrLn "Request failed"
      print e
    Right Response { result = m } -> do
      putStrLn "Request succeded"
      print $ message_id m
      print $ text m
  where token = Token "bot788362389:AAEGzjN2HXWE-fwF_-m56lYhfGfMMZYj9-0"
        chatId = ChatId 113313863 -- use ChatId 10231 or ChatChannel "<@channelusername>"
        message = "text *bold* _italic_ [github](github.com/ratijas/haskell-telegram-api)"

spec :: Token -> ChatId -> Text -> Spec
spec token chatId botName = do
  manager <- runIO $ newManager tlsManagerSettings
  dataDir <- runIO getDataDir
  let testFile name = dataDir </> "test-data" </> name
  describe "/sendMessage" $ do
    it "should send message" $ do
      res <- sendMessage token (sendMessageRequest chatId "test message") manager
      success res
      let Right Response { result = m } = res
      text m `shouldBe` Just "test message"
      res <- sendMessage token request manager
      success res
      let Right Response { result = m } = res
      text m `shouldBe` Just "text bold italic github"

    it "should set keyboard" $ do
      let kbA = keyboardButton "A"
          kbB = keyboardButton "B"
          kbC = keyboardButton "C"
      let message = (sendMessageRequest chatId "set keyboard") {
        message_reply_markup = Just $ replyKeyboardMarkup [[kbA, kbB, kbC]]
      }
      res <- sendMessage token message manager
      success res
      let Right Response { result = m } = res
      text m `shouldBe` Just "set keyboard"

    it "should remove keyboard" $ do
      let message = (sendMessageRequest chatId "remove keyboard") {
        message_reply_markup = Just replyKeyboardHide
      }
      res <- sendMessage token message manager
      success res
      let Right Response { result = m } = res
      text m `shouldBe` Just "remove keyboard"
      