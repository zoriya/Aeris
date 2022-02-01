{-# language OverloadedStrings #-}
module Api.User where

import Db.User


users :: [User']
users = [
  User
    { userId = 1
    , username = "admin"
    , password = "passwd"
    , slug = "admin"
    }
  ]
