{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Categorias where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql

putCategoriaIdR :: EventosCategoriasId -> Handler Value
putCategoriaIdR cid = do
    _ <- runDB $ get404 cid
    novaCat <- requireJsonBody :: Handler EventosCategorias
    runDB $ replace cid novaCat
    sendStatusJSON noContent204 (object [])

deleteCategoriaIdR :: EventosCategoriasId -> Handler Value
deleteCategoriaIdR cid = do
    _ <- runDB $ get404 cid
    runDB $ delete cid
    sendStatusJSON noContent204 (object [])

getCategoriaIdR :: EventosCategoriasId -> Handler Value
getCategoriaIdR cid = do
    categoria <- runDB $ get404 cid
    sendStatusJSON ok200 (object ["resp" .= categoria])

getCategoriaR :: Handler Value
getCategoriaR = do
    todasCat <- runDB $ selectList [] [Asc EventosCategoriasDescricao]
    sendStatusJSON ok200 (object ["resp" .= todasCat])

postCategoriaR :: Handler Value
postCategoriaR = do
    categoria <- requireJsonBody :: Handler EventosCategorias
    cid <- runDB $ insert categoria
    sendStatusJSON created201 (object ["resp" .= fromSqlKey cid])
