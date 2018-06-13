{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Eventos where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql

getEventoIdR :: EventosId -> Handler Value
getEventoIdR eid = do
    evento <- runDB $ get404 eid
    sendStatusJSON ok200 (object ["resp" .= evento])

-----------------------------------------------------------------------------------------------------------------------------------------------

getEventosPorCidadeR :: CidadesId -> Handler Value
getEventosPorCidadeR cid = do
    eventosDaCidade <- runDB $ selectList [EventosCidadeid ==. cid] []
    sendStatusJSON ok200 (object ["resp" .= eventosDaCidade])

getEventosPorCategoriaR :: EventosCategoriasId -> Handler Value
getEventosPorCategoriaR cid = do
    eventosDaCategoria <- runDB $ selectList [EventosEventocategoriaid ==. cid] []
    sendStatusJSON ok200 (object ["resp" .= eventosDaCategoria])

getEventosPorCidadeCategoriaR :: CidadesId -> EventosCategoriasId -> Handler Value
getEventosPorCidadeCategoriaR cidid catid = do
    eventosResult <- runDB $ selectList [EventosCidadeid ==. cidid, EventosEventocategoriaid ==. catid] []    
    sendStatusJSON ok200 (object ["resp" .= eventosResult])

-----------------------------------------------------------------------------------------------------------------------------------------------

--getEventosInteresseUsuarioR :: UsuariosId -> Handler Value
--getEventosInteresseUsuarioR uid = do
    --usuarioInteracoes <- runDB $ selectList [UsuariosInteracoesUsuarioid ==. uid, UsuariosInteracoesInteracaotipoid ==. 1] []
    --usuarioInteresses <- return $ map (eventosId . entityVal ) usuarioInteracoes
    --eventosInteresse <- runDB $ selectList [EventosId <-. usuarioInteresses] []    
    --sendStatusJSON ok200 (object ["resp" .= eventosInteresse])

--getEventosComparecerUsuarioR :: UsuariosId -> Handler Value
--getEventosComparecerUsuarioR uid = do
    --usuarioInteracoes <- runDB $ selectList [UsuariosInteracoesUsuarioid ==. uid] []
    --usuarioInteresses <- return $ map (eventosEventoId . entityVal ) usuarioInteracoes
    --eventosInteresse <- runDB $ selectList [EventosId <-. usuarioInteresses] []    
    --sendStatusJSON ok200 (object ["resp" .= eventosInteresse])    

-----------------------------------------------------------------------------------------------------------------------------------------------

putEventoIdR :: EventosId -> Handler Value
putEventoIdR eid = do
    _ <- runDB $ get404 eid
    eventoAlteracao <- requireJsonBody :: Handler Eventos
    runDB $ replace eid eventoAlteracao
    sendStatusJSON noContent204 (object [])

deleteEventoIdR :: EventosId -> Handler Value
deleteEventoIdR eid = do
    _ <- runDB $ get404 eid
    runDB $ delete eid
    sendStatusJSON noContent204 (object [])

-----------------------------------------------------------------------------------------------------------------------------------------------    

postEventoR :: Handler Value
postEventoR = do
    evento <- requireJsonBody :: Handler Eventos
    eid <- runDB $ insert evento
    sendStatusJSON created201 (object ["resp" .= fromSqlKey eid])
