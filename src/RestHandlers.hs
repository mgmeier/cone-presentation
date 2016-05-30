{-# LANGUAGE OverloadedStrings, CPP, LambdaCase #-}

module  RestHandlers (restHandlers, doesPresentationExist) where

import  ConeServer.RestAPI
import  ConeServer.ConeTypes
import  ConeServer.Utils
import  Modules
import  ConeDemoImport

import  ConeServer.PresentationApp.Presentation_V2
import  Data.Aeson              (decode')
import  Data.ByteString.Lazy    (fromStrict)
import  System.Directory        (doesFileExist)
import  qualified Data.ByteString.Char8    as BSC (pack)
#ifndef RELEASE
import  qualified Data.ByteString.Lazy.Char8    as BL (pack)
#endif


restHandlers :: RestAPI
restHandlers = RestAPI "ConePresentation"
    [ RestRedirect  "initialize and start presentation"
        ["present"] loadPresentation
    , RestJSON "expects a ConePresentationEvent JSON; responds with a ConePresentationFrame JSON"
        ["present", "event"] handlerPresentationEvent
    , RestEmpty "expects a location update (selection? Url?)"
        ["present", "navi"] handlerPresentationNavi
#ifndef RELEASE
    , RestText "print current presentation state"
        ["present", "show"] handlerShowPres
#endif
    , RestStatic "the path for the presentation's .html, .js and .css files"
        presentationSubDir
    ]


handlerPresentationEvent :: RestHandlerJSON (ConeFrame CCAnimation)
handlerPresentationEvent = RestHandler $ \env (_, ioLocal) -> case env of
    (POST, params, _) ->
        let
            mEvent :: Maybe ConePresentationEvent
            mEvent = getParamByteString "data" params >>= decode' . fromStrict
        in readMVar ioLocal >>= \l -> case mEvent of
            Just ev
                | (not . null) (cprOrig . testPresentation $ l) ->
                    modifyMVar ioLocal $ \local -> do
                        (frame, l') <- processPresentationEvent local ev
                        return (l', Just frame)
            _ -> return Nothing
    _ -> return Nothing


handlerPresentationNavi :: RestHandlerEmpty
handlerPresentationNavi = RestHandler $ \env (g, ioLocal) -> case env of
    (POST, params, _) ->
        -- TODO check if url is part of forward, then backward frame
        -- if so, update presentation index and selection
        -- TODO if selection: update index and selection
        return True
    _ -> return False


loadPresentation :: RestHandlerRedirect
loadPresentation = RestHandler $ \_ (_, ioLocal) ->
    loadPresentationFiles >>= \case
        Left err  -> putStrLn ("<ERROR> loadPresentation: " ++ err) >> return Nothing
        Right (m, pres) -> do
            mPrefs <- serverLoadFile presentationSubDir "default.json"
            modifyMVar_ ioLocal $ \l -> return l
                { testPresentation  = pres {cprSel = presentationInitialSelection}
                , testModel         = m
                , testSelection     = presentationInitialSelection
                , testPreferences   = fromMaybe (testPreferences l) (serverBuildPreferences <$> mPrefs)
                }
            return $ Just presentationRoot
  where
    presentationRoot = "/" <> BSC.pack presentationSubDir <> "/html/index.html"

loadPresentationFiles :: IO (Either String (ConeTree, ConePresentation))
loadPresentationFiles = do
    fModel <- maybe (Left $ "model file not found: " ++ fNameModel) Right
            <$> serverLoadFile presentationSubDir fNameModel
    fScrpt <- maybe (Left $ "script file not found: " ++ fNameScript) Right
            <$> serverLoadFile presentationSubDir fNameScript
    let
        pModel = do
            m <- fromStrict <$> fModel
            decodeConeTree m
        pScript = do
            s <- fromStrict <$> fScrpt
            fromScriptJSON s

    return $ case (pModel, pScript) of
        (Right m, Right pres) -> Right (m, pres)
        _ -> Left $ unlines $ fromLeft pModel ++ fromLeft pScript
  where
    fromLeft    = either (:[]) (const [])
    fNameModel  = "model.json"
    fNameScript = "script.json"

#ifndef RELEASE
handlerShowPres :: RestHandlerText
handlerShowPres = RestHandler $ \_ (_, ioLocal) -> do
    pres <- testPresentation  <$> readMVar ioLocal
    let
        msg = unlines
            [ "presentation state: "
            , ""
            , show pres
            ]
    return $ BL.pack msg
#endif
