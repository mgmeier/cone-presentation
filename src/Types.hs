{-# LANGUAGE CPP, StandaloneDeriving, OverloadedStrings, LambdaCase, DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, RecordWildCards #-}


module  Types
        ( module Types
        , module Control.Monad
        ) where

import  GHC.Generics                    (Generic)
import  ConeServer.Types
import  ConeServer.ConeTypes            (ConeTree, ConeSelection(..), emptyConeSelection)

import  Data.Aeson                      as Ae
import  Data.Text                       as T (Text, unpack, pack)
import  qualified Data.ByteString       as BS (tail, init)
import  qualified Data.ByteString.Lazy  as BL (ByteString)

import  Control.Monad
import  Control.Applicative


presentationSubDir :: FilePath
presentationSubDir = "presentation"

presentationInitialSelection :: ConeSelection
presentationInitialSelection = ConeSelection False [0]


data PresSelect
    = PresSelPath   Bool SelectionPath
    | PresSelTextId Bool Text
    | PresSelUrl    Bool Text

data ConeFrame a = ConeFrame
    { cfAnims   :: [a]
    , cfSel     :: Maybe PresSelect
    , cfUrl     :: Maybe Text
    , cfComment :: Maybe Text
    }
    | EmptyFrame

instance FromJSON PresSelect where
    parseJSON (Object o) = do
        isParent <- o .:? "selIsParent" .!= False
        (PresSelPath isParent <$> o .: "selPath")
            <|> (PresSelTextId isParent <$>  o .: "selTextId")
            <|> (PresSelUrl isParent <$>  o .: "selUrl")
    parseJSON _ = mzero


instance FromJSON a => FromJSON (ConeFrame a) where
    parseJSON obj@(Object o) =
        let
            mSelection :: Maybe PresSelect
            mSelection = case fromJSON obj of
                Success a   -> Just a
                _           -> Nothing
        in ConeFrame
            <$> o .: "animations"
            <*> pure mSelection
            <*> o .:? "urlExpl"
            <*> o .:? "comment"
    parseJSON _ = mzero

instance ToJSON a => ToJSON (ConeFrame a) where
    toJSON (ConeFrame anims _ url comment) = object $
        [ "tag"         .= ("ConeFrame" :: String)
        , "animations"  .= anims
        ]
        ++ maybe [] (\val -> ["urlExpl" .= val]) url
        ++ maybe [] (\val -> ["comment" .= val]) comment
        -- TODO selection
    toJSON _ = Null


isEmptyFrame :: ConeFrame a -> Bool
isEmptyFrame = \case {EmptyFrame -> True; _ -> False}
{-# INLINE isEmptyFrame #-}

data ConePresentation = ConePresentation
    { cprOrig       :: [ConeFrame CPAnimation]
    , cprDone       :: ([ConeFrame CPAnimation], [ConeFrame CPAnimation])
    , cprUrl        :: Maybe Text
    , cprSel        :: ConeSelection
    }

emptyConePresentation :: ConePresentation
emptyConePresentation = ConePresentation [] ([], []) Nothing emptyConeSelection


newtype ScriptJSON = ScriptJSON {unScriptJSON :: [ConeFrame CPAnimation]}

instance FromJSON ScriptJSON where
    parseJSON arr@Array {} = ScriptJSON <$> parseJSON arr
    parseJSON _ = mzero

fromScriptJSON :: BL.ByteString -> Either String ConePresentation
fromScriptJSON =
    eitherDecode' >=> return . \ok -> let fs = unScriptJSON ok in
        ConePresentation fs ([], fs) Nothing presentationInitialSelection


data ConePresentationEvent
    = CPENext
    | CPEPrevious
    | CPEStart
    | CPEEnd
    | CPEExposeCanvas
    | CPEHideCanvas
    deriving (Show, Enum, Bounded)

instance FromJSON ConePresentationEvent where
    parseJSON (Ae.String s) =
        maybe mzero return (lookup s allEvents)
      where
        allEvents :: [(Text, ConePresentationEvent)]
        allEvents = [(T.pack $ show ev, ev) | ev <- [minBound .. maxBound]]
    parseJSON _ = mzero


data CPAnimation
    = CPAMoveLeft                 Int
    | CPAMoveRight              Int
    | CPAMoveUp
    | CPAMoveDown
    | CPAClockworkUrl           Text
    | CPAClockworkTextId        Text
    | CPAClockworkPath          SelectionPath
    | CPATransformLeft
    | CPATransformRight
    | CPATransformIntro         {newModel :: Either FilePath ConeTree, right :: Bool}
        -- ^ Introduces a new subtree. If The Boolean is true to the right, else to the left of
        -- the current selection
    -- | CPATransformExtro Boolean
    | CPATransformExtro         {right :: Bool}
        -- ^ Removes the current item.
        -- if the Boolean is true, make the item to the left the new current item, if false the right
    | CPATransformDown
    -- | CPATransformDown2 (ConeZip a)
        -- ^ The current item will be pushed down to the tree to the right
    | CPATransformUp -- (ConeZip a)
        -- ^ The current item will be pushed up one level, and inserted before the current position
        -- in the upper tree.
    | CPATransformShiftToZero -- (ConeZip a)
        -- ^ The current will get index 0
    | CPATransformReplace       {newModel :: Either FilePath ConeTree, newPath :: Maybe SelectionPath}
        -- ^ Sets a whole new model.
        -- NB only the Left case is regarded for newModel. Specify as follows (newPath being optional):
        {-
            {
            	"tag":       "CPATransformReplace",
            	"newModel":  {"Left": "test.json"},
                "newPath":   [0,2,3]
            }
        -}
    | CPAGlobalView             Bool
        -- ^ true for starting, false for finishing
    | CPAZoom                   (Maybe Float)
        -- ^ Number stands for the end zoom
    | CPAPause                  Int
        -- No CPAation, and wait for Number milliseconds before continue
    | CPAExposeCanvas
    | CPAHideCanvas
    | CPACut
    | CPACopy
    | CPAPaste
    | CPANull
    deriving Generic

instance FromJSON CPAnimation

{-
instance ToJSON CPAnimation
test1 = CPATransformReplace (Left "test.json") Nothing      :: CPAnimation
test2 = CPATransformReplace (Left "test.json") (Just [0])   :: CPAnimation
testtest1 = print (encode test1) >> print (encode test2)
testtest2 = decode "{\"tag\":\"CPATransformReplace\",\"newModel\":{\"Left\":\"test.json\"}}" :: Maybe CPAnimation
-}

-- orphan instance needed only for parsing the (Either FilePath ConeTree) arguments
-- mzero, because only model filenames are allowed as of now
instance FromJSON ConeTree where
    parseJSON = const mzero


#ifndef RELEASE
deriving instance Show a => Show (ConeFrame a)
deriving instance Show CPAnimation
deriving instance Show PresSelect

instance Show ConePresentation where
    show (ConePresentation orig (done, next) mUrl coneSel) = unlines
        [ "ConePresentation"
        , "  original frames: " ++ show orig
        , ""
        , "  no. of frames:   " ++ show (length orig)
        , ""
        , "  done frames:     " ++ show done
        , ""
        , "  next frames:     " ++ show next
        , ""
        , "  url remembered:  " ++ show mUrl
        , "  selection:       " ++ show coneSel
        ]

{-
-- generate a Haskell definition of some presentation (e.g. a parse from JSON)
haskellize :: ConePresentation -> String
haskellize ConePresentation {cprOrig = fs} =
    "TODO"                                              -- TODO
  where
    showFrame EmptyFrame = "[]"
    showFrame ConeFrame {..} = "    , " ++ show cfAnims ++ maybe "" (("-- " ++) . T.unpack) cfComment
-}

#endif
