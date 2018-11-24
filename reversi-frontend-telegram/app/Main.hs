{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import           Network.HTTP.Client      (newManager)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Web.Telegram.API.Bot
import           Data.Text as T

main :: IO ()
main = sendM

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
  let request = sendMessageRequest' chatId message
  res <- sendMessage token request manager
  case res of
    Left e -> do
      putStrLn "Request failed"
      print e
    Right Response { result = m } -> do
      putStrLn "Game on"
      print $ message_id m
      print $ text m
  where token = Token "bot788362389:AAEGzjN2HXWE-fwF_-m56lYhfGfMMZYj9-0"
        chatId = ChatId 113313863
        message = "send works"



sendMessageRequest' :: ChatId -> Text -> SendMessageRequest
sendMessageRequest' id text =
  let req = sendMessageRequest id text
      buttons =  [ [ (inlineKeyboardButton " ") { ikb_callback_data = Just $ pack (show (i, j)) } | i <- [1..8] ] | j <- [1..8] ]
      markup = ReplyInlineKeyboardMarkup  buttons
    in req { message_reply_markup = Just markup }


