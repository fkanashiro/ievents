{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Usuarios where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql

getUsuarioIdR :: UsuariosId -> Handler Value
getUsuarioIdR uid = do
    usuario <- runDB $ get404 uid
    sendStatusJSON ok200 (object ["resp" .= usuario])

postUsuarioR :: Handler Value
postUsuarioR = do
    usuario <- requireJsonBody :: Handler Usuarios
    uid <- runDB $ insert usuario
    sendStatusJSON created201 (object ["resp" .= fromSqlKey uid])    