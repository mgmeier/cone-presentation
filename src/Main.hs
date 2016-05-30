{-# LANGUAGE OverloadedStrings #-}

import Types

import ConeServer.RunServer
import ConeServer.ConeTypes
import ConeServer.Types
import ConeServer.Utils         (SessionLocal, emptySessionLocal)

import Network.Wai.Handler.Warp (Port)
import System.Directory         (getCurrentDirectory, doesFileExist)

import Data.List                (intercalate)

srvPort :: Port
srvPort = 8091


-- TODO scan multiple subdirectories for presentations / have multiple html roots

doesPresentationExist :: IO Bool
doesPresentationExist = do
    check <- and <$> sequence
        [ doesFileExist fullPath
            | f <- mandatoryFiles
            , let fullPath = concat [presentationSubDir, "/", f]
        ]
    unless check $ putStrLn $ unlines
        [ "WARNING: presentation data not found"
        , "    - maybe create a soft-link to some presentation folder?"
        , "      e.g. 'ln -s /path/to/ConeCanvasPresentation " ++ presentationSubDir ++ "'"
        , "    - check for presence of all mandatory files in the presentation folder:"
        , "     " ++ intercalate ", " mandatoryFiles
        ]
    return check
  where
    mandatoryFiles = ["model.json", "script.json", "html/index.html"]


main, main' :: IO ()
main = do
    putStrLn "ConePresentation framework (c) Symbolian GmbH 2016"
    ok <- doesPresentationExist
    when ok main'

main' = do
    baseDir <- getCurrentDirectory

    token@(sessGlobal, _) <- initServer srvPort baseDir False

    putStrLn $ "starting on localhost:" ++ show srvPort

    let
        initUserSession :: IO (SessionLocal ConePresentation)
        initUserSession = return $
            emptySessionLocal sessGlobal emptyConePresentation

    runServer token "PRESENTAPP" Nothing Nothing initUserSession
