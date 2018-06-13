{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Interacoes where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql

patchInteracaoTipoR :: UsuariosInteracoesId -> Int -> Handler Value
patchInteracaoTipoR uiid tipo = do
    _ <- runDB $ get404 uiid
    runDB $ update uiid [UsuariosInteracoesInteracaotipoid =. tipo]
    sendStatusJSON noContent204 (object [])

deleteInteracaoIdR :: UsuariosInteracoesId -> Handler Value
deleteInteracaoIdR iid = do
    _ <- runDB $ get404 iid
    runDB $ delete iid
    sendStatusJSON noContent204 (object [])

postInteracaoR :: Handler Value
postInteracaoR = do
    interacao <- requireJsonBody :: Handler UsuariosInteracoes
    iid <- runDB $ insert interacao
    sendStatusJSON created201 (object ["resp" .= fromSqlKey iid])

--optionsInteracaoTipoR :: Handler UsuariosInteracoes
--optionsInteracaoTipoR = do
    --addHeader "Access-Control-Allow-Origin" "*"
    --addHeader "Access-Control-Allow-Methods" "PATCH, OPTIONS"
    --addHeader "Access-Control-Allow-Headers" "Origin, X-Requested-With, Content-Type, Accept"
    --return $ UsuariosInteracoes $ toContent ("" :: Text)