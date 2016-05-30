{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase #-}

module  Presentation_V2
        ( processPresentationEvent
        , module ConeServer.PresentationApp.Types
        ) where

import  ConeServer.PresentationApp.Types
import  ConeServer.Types                as Types
import  ConeServer.ConeTypes
import  ConeServer.Utils
import  Modules
import  ConeUtils
import  ConeDemoImport

import  qualified Data.Text             as T (isSuffixOf, empty)
import  Data.ByteString.Lazy            (fromStrict)
import  Control.Monad.State


prependAnims :: [a] -> ConeFrame a -> ConeFrame a
prependAnims anims (ConeFrame a b c d) = ConeFrame (anims ++ a) b c d
prependAnims _      EmptyFrame         = EmptyFrame


clockworkToSelection :: Maybe PresSelect -> Maybe CPAnimation
clockworkToSelection = fmap $ \case
    PresSelPath _ p     -> CPAClockworkPath p
    PresSelTextId _ t   -> CPAClockworkTextId t
    PresSelUrl _ u      -> CPAClockworkUrl u


loadPresentationModel :: FilePath -> IO ConeTree
loadPresentationModel fp = serverLoadFile "presentation" fp >>= \case
    Nothing -> loadError $ fp ++ " not found"
    Just f  -> case (decodeConeTree . fromStrict) f of
        Left e  -> loadError $ e ++ " when decoding " ++ fp
        Right t -> return t
  where
    loadError e = putStrLn ("loadPresentationModel: " ++ e) >> return emptyTree


-- for all animations that require pre-loading / -processing an additional model
preprocessConeFrame :: ConeFrame CPAnimation -> IO (ConeFrame CPAnimation)
preprocessConeFrame (ConeFrame anims b c d) =
    mapM preprocessAnimation anims >>= \anims' -> return $ ConeFrame anims' b c d
  where
    preprocessAnimation (CPATransformReplace (Left fp) mPath) =
        flip CPATransformReplace mPath . Right <$> loadPresentationModel fp
    preprocessAnimation (CPATransformIntro (Left fp) r) =
        flip CPATransformIntro r . Right <$> loadPresentationModel fp
    preprocessAnimation a = return a


-- TODO
invertConeframe :: ConeFrame CPAnimation -> ConeFrame CPAnimation -> ConeFrame CPAnimation
invertConeframe current previous = undefined

invertAnim :: CPAnimation -> CPAnimation
invertAnim (CPAMoveRight i)             = CPAMoveLeft i
invertAnim (CPAMoveLeft i)              = CPAMoveRight i
invertAnim CPAMoveUp                    = CPAMoveDown
invertAnim CPAMoveDown                  = CPAMoveUp
invertAnim cl@(CPAClockworkUrl _)       = cl
invertAnim cl@(CPAClockworkTextId _)    = cl
invertAnim cl@(CPAClockworkPath _)      = cl
invertAnim CPATransformLeft             = CPATransformRight
invertAnim CPATransformRight            = CPATransformLeft
-- TODO
invertAnim CPAHideCanvas                = CPAExposeCanvas
invertAnim CPAExposeCanvas              = CPAHideCanvas
-- invertAnim _ = CPANull


processPresentationEvent :: SessionLocal b -> ConePresentationEvent -> IO (ConeFrame CCAnimation, SessionLocal b)
processPresentationEvent sess =
    process
  where
    pres @ ConePresentation {..} = testPresentation sess

    process CPENext = case cprDone of
        (ys, x:xs) -> do
            x' <- preprocessConeFrame x
            let
                (frame, undo, sess') = evaluateFrame sess x'
                pres' = pres {cprDone = (x:ys, xs)}
            return (frame, sess' {testPresentation = pres'})
        _ -> return (EmptyFrame, sess)
    process CPEPrevious = case cprDone of
        (y:ys, xs) ->
            let
                (frame, _, sess')   = evaluateFrame sess y
                pres'               = pres {cprDone = (ys, y:xs)}
            in return (frame, sess' {testPresentation = pres'})
        _ -> return (EmptyFrame, sess)
    process CPEStart =
        let
            pres'       = pres {cprDone = ([], cprOrig)}
        -- TODO evaluate first frame, or generate clockwork to initialSel? reset model?
        in return (EmptyFrame, sess {testPresentation = pres'})
    process CPEEnd = do
        let
            fs@(f:_)    = reverse cprOrig
            pres'       = pres {cprDone = (fs, [])}
        -- TODO evaluate last frame
        return (EmptyFrame, sess {testPresentation = pres'})

    process CPEHideCanvas =
        return (ConeFrame [AnimNull] Nothing Nothing Nothing, sess)             -- TODO Anim.. value
    process CPEExposeCanvas =
        return (ConeFrame [AnimNull] Nothing Nothing Nothing, sess)             -- TODO Anim... value


evaluateFrame :: SessionLocal b -> ConeFrame CPAnimation
    -> (ConeFrame CCAnimation, ConeFrame CPAnimation, SessionLocal b)
evaluateFrame sess EmptyFrame = (EmptyFrame, EmptyFrame, sess)
evaluateFrame sess (ConeFrame anims sel url c) =
    ( ConeFrame anims' sel url c
    , ConeFrame (reverse inv) (Just resultSel) previousUrl c
    , sess'
    )
  where
    f st anim       = swap $ runState (evAnim anim) st
    (sess', result) = mapAccumL f sess anims
    (anims', inv)   = unzip result
    previousUrl     = cprUrl . testPresentation $ sess
    resultSel       = PresSelPath False (selectionPath $ testSelection sess')


-- evAnim does not modify SessionLocal's ConeSelection, only presentation's
evAnim :: CPAnimation -> State (SessionLocal ConePresentation) CCAnimation
evAnim CPAMoveUp = do
    pres <- gets slData
    let sel@(ConeSelection _ p) = cprSel pres
    if null p
        then return AnimNull
        else do
            let sel' = modifyPath (init p) sel
            evUpdatePresentation pres {cprSel = sel'}
            return AnimMoveUp

evAnim CPAMoveDown = do
    sel <- gets testSelection
    m   <- gets testModel
    let
        p                   = selectionPath sel
        (RoseLeaf _ ix xs)  = selectByPath p m
    when (ix >= 0 && (not . null) xs) $ do
        modify' (\st -> st {testSelection = modifyPath (p ++ [ix]) sel})
        -- evAnimSessionPathToPresPath
    return AnimMoveDown

evAnim (CPAMoveLeft i) =
    evAnimHelperMoveLeftRight ixFunc (AnimMoveLeft i, CPAMoveRight i)
  where
    ixFunc len ix = let t = ix - i in if t < 0 then t + len else t

evAnim (CPAMoveRight i) =
    evAnimHelperMoveLeftRight ixFunc (AnimMoveRight i, CPAMoveLeft i)
  where
    ixFunc len ix = let t = ix + i in if t >= len then t - len else t

evAnim CPATransformShiftToZero = do
    p <- selectionPath <$> gets testSelection
    -- let retCons m = (AnimTransformShiftToZero $ Just m, CPAMoveRight $ last p) -- TODO action: select first child?
    let retCons m = (AnimTransformShiftToZero Nothing, CPAMoveRight $ last p)
    evAnimHelperTransLeftRightShift ixFunc retCons
  where
    ixFunc _ _  = 0

evAnim CPATransformLeft =
    evAnimHelperTransLeftRightShift ixFunc retCons
  where
    -- retCons m       = (AnimTransformLeft $ Just m, CPATransformRight)
    retCons m       = (AnimTransformLeft Nothing, CPATransformRight)
    ixFunc len ix   = let t = ix - 1 in if t < 0 then len - 1 else t

evAnim CPATransformRight =
    evAnimHelperTransLeftRightShift ixFunc retCons
  where
    -- retCons m       = (AnimTransformRight $ Just m, CPATransformLeft)
    retCons m       = (AnimTransformRight Nothing, CPATransformLeft)
    ixFunc len ix   = let t = ix + 1 in if t >= len then 0 else t

evAnim (CPAClockworkPath p) = do
    sess <- get
    let
        pres    = testPresentation sess
        pres'   = pres  -- {cprSelPath = p}
    put $ sess {testPresentation = pres', testSelection = ConeSelection False p}
    return (AnimClockwork p, CPAClockworkPath [0]) -- (cprSelPath pres))        -- FIXME

evAnim (CPAClockworkTextId t) = do
    m <- gets testModel
    let
        mPath = findSelection ((== t) . ceTextId) m                             -- TODO autocomplete base prefix
            <|> findSelection ((== t) . ceLabel) m
    (evAnim . maybe CPANull CPAClockworkPath) mPath


evAnim (CPAClockworkUrl t) = do
    m <- gets testModel
    (evAnim . maybe CPANull CPAClockworkPath) $
        findSelection ((t `T.isSuffixOf`) . fromMaybe T.empty . ceTargetUri) m

evAnim (CPATransformReplace ~(Right m') mPath) = do
    st @ SessionLocal {testModel = m, testSelection = path} <- get
    let
        m'' = enumerateTree coneEntrySetId (1 + maxEntryId m) m'
        p   = fromMaybe [0] mPath
    put st
        { testModel     = Types.setSelectionPath p m''
        , testSelection = ConeSelection False p
        }
    return $ AnimTransformReplace (toSubTree False m'') mPath
        -- , CPATransformReplace (Right m) (Just $ selectionPath path)

evAnim (CPATransformIntro ~(Right m') r) = do
    st @ SessionLocal {testModel = tree, testSelection = path} <- get
    let
        selPath = selectionPath path
        m''     = enumerateTree coneEntrySetId (1 + maxEntryId tree) m'
        tree'   = insertConeTree r selPath m'' tree
        tree''  = updateConeIds selPath tree'
    put st {testModel = tree''}
    return $ AnimTransformIntro r (toSubTree False m'') Nothing -- (Just $ toSubTree True tree'')

evAnim (CPATransformExtro r) = do
    st @ SessionLocal {testModel = tree, testSelection = path} <- get
    let
        selPath          = selectionPath path
        (tree', deleted) = extractConeTree selPath tree
        tree''           = updateConeIds (init selPath) tree'
        RoseLeaf _ _ cs  = selectByPath (init selPath) tree''
        newIx            = if r then last selPath - 2 else last selPath
        newIx'           = newIx `mod` length cs
        selPath'         = if null cs then init selPath else init selPath ++ [newIx']
    put st
        { testModel     = Types.setSelectionPath selPath' tree''
        , testSelection = path {selectionPath = selPath'}
        }
    return $ AnimTransformExtro r Nothing -- (Just $ toSubTree True tree'')


evAnim (CPAGlobalView b)    = return (AnimGlobalView b, CPAGlobalView $ not b)
evAnim (CPAZoom f)          = return (AnimZoom f, CPAZoom Nothing)
evAnim p@(CPAPause i)       = return (AnimPause i, p)
evAnim CPAHideCanvas        = return (AnimNull, CPAExposeCanvas)                -- TODO Anim... value
evAnim CPAExposeCanvas      = return (AnimNull, CPAHideCanvas)                  -- TODO Anim... value
evAnim CPACut               = return (AnimCut, CPANull)                         -- TODO synchronize with SessionLocal
evAnim CPACopy              = return (AnimCopy, CPANull)                        -- TODO synchronize with SessionLocal
evAnim CPAPaste             = return (AnimPaste, CPANull)                       -- TODO synchronize with SessionLocal
evAnim _                    = return (AnimNull, CPANull)


-- avoid code duplication for cases moveleft / moveright
evAnimHelperMoveLeftRight :: (Int -> Int -> Int) -> CCAnimation -> State (SessionLocal ConePresentation) CCAnimation
evAnimHelperMoveLeftRight ixFunc retVal = do
    sel     <- gets testSelection
    m       <- gets testModel
    let p_  = selectionPath sel
    unless (null p_) $
        let
            (p, ix)             = (init &&& last) p_
            (RoseLeaf _ _ xs)   = selectByPath p m
        in unless (null xs) $
            let
                p'  = p ++ [ixFunc (length xs) ix]
                m'  = Types.setSelectionPath p' m
            in do
                modify' (\st -> st {testSelection = modifyPath p' sel, testModel = m'})
                -- evAnimSessionPathToPresPath
    return retVal


-- avoid code duplication for cases transform left / right / shift to zero
evAnimHelperTransLeftRightShift :: (Int -> Int -> Int) -> (ConeSubtree -> CCAnimation) -> State (SessionLocal b) CCAnimation
evAnimHelperTransLeftRightShift ixFunc retCons = do
    p       <- selectionPath <$> gets testSelection
    if null p
        then retCons . toSubTree True <$> gets testModel
        else do
            m       <- gets testModel
            let m'  = updateConeIds (init p) $ transformChildOrder ixFunc p m
            modify' (\st -> st {testModel = m'})
            return  (retCons $ toSubTree True m')


evUpdatePresentation :: ConePresentation -> State (SessionLocal ConePresentation) ()
evUpdatePresentation pres =
    modify' (\st -> st {slData = pres})
{-# INLINE evUpdatePresentation #-}

-- updates the selection saved in the presentation with the one from the user's session
evAnimSessionPathToPresPath :: State (SessionLocal ConePresentation) ()
evAnimSessionPathToPresPath = do
    pres    <- gets slData
    sel     <- gets testSelection
    evUpdatePresentation pres {cprSel = sel}
