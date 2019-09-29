{-# LANGUAGE PatternSynonyms, ViewPatterns, RecordWildCards, CPP, MultiParamTypeClasses, TypeFamilies, DeriveGeneric, DeriveAnyClass, OverloadedStrings, FlexibleContexts #-}
module Pure.Visibility where

import Pure hiding (Visibility,features,children)

import Pure.Data.Cond
import Pure.Data.Prop

import Control.Arrow ((&&&))
import Control.Exception (handle,SomeException)
import Control.Monad (unless,void,when)
import Data.Coerce
import Data.Foldable (for_,traverse_)
import Data.IORef
import Data.Maybe
import GHC.Generics as G

import Data.Function ((&))

import Pure.Data.Txt

data Passed = PixelsPassed Double | PercentPassed Double
    deriving (Generic,Default,Ord,Eq)

data Visibility = Visibility_
    { as                     :: Features -> [View] -> View
    , features               :: Features
    , children               :: [View]
    , context                :: Maybe JSV
    , continuous             :: Bool
    , fireOnMount            :: Bool
    , offset                 :: (Double,Double)
    , onBottomPassed         :: Maybe (Calculations -> IO ())
    , onBottomPassedReverse  :: Maybe (Calculations -> IO ())
    , onBottomVisible        :: Maybe (Calculations -> IO ())
    , onBottomVisibleReverse :: Maybe (Calculations -> IO ())
    , once                   :: Bool
    , onOffScreen            :: Maybe (Calculations -> IO ())
    , onOnScreen             :: Maybe (Calculations -> IO ())
    , onPassed               :: [(Calculations -> IO (),Passed)]
    , onPassing              :: Maybe (Calculations -> IO ())
    , onPassingReverse       :: Maybe (Calculations -> IO ())
    , onTopPassed            :: Maybe (Calculations -> IO ())
    , onTopPassedReverse     :: Maybe (Calculations -> IO ())
    , onTopVisible           :: Maybe (Calculations -> IO ())
    , onTopVisibleReverse    :: Maybe (Calculations -> IO ())
    , onUpdate               :: Maybe (Calculations -> IO ())
    } deriving (Generic)

instance Default Visibility where
    def = (G.to gdef) { as = \fs cs -> Div & Features fs & Children cs, context = Just (coerce window), once = True }

pattern Visibility :: Visibility -> Visibility
pattern Visibility v = v

data VisibilityState = VS
    { oldCalculations :: IORef Calculations
    , calculations    :: IORef Calculations
    , verticalOffset  :: IORef Int
    , handlers        :: IORef VisibilityHandlers
    , fired           :: IORef [Txt]
    , ticking         :: IORef Bool
    , ref             :: IORef (Maybe JSV)
    }

data VisibilityHandlers = VH
    { resizeHandler :: IO ()
    , scrollHandler :: IO ()
    } deriving (Generic,Default)

data Direction = Down | Up
    deriving (Generic,Default,Eq,Ord,Show)
instance ToTxt Direction where
    toTxt Up = "up"
    toTxt _  = "down"
instance FromTxt Direction where
    fromTxt "up" = Up
    fromTxt _    = Down

data Calculations = Calculations
    { direction        :: Direction
    , height           :: Double
    , width            :: Double
    , top              :: Double
    , bottom           :: Double
    , percentagePassed :: Double
    , pixelsPassed     :: Double
    , bottomPassed     :: Bool
    , bottomVisible    :: Bool
    , fits             :: Bool
    , passing          :: Bool
    , offScreen        :: Bool
    , onScreen         :: Bool
    , topPassed        :: Bool
    , topVisible       :: Bool
    } deriving (Generic,Default)

instance Pure Visibility where
    view =
        LibraryComponentIO $ \self ->
            let
                handleRef (Node n) = do
                    VS {..} <- get self
                    writeIORef ref (Just n)

                execute Nothing _ = return ()
                execute (Just callback) name = do
                    Visibility_ {..} <- ask self
                    VS          {..} <- get self

                    cs <- readIORef calculations
                    fs <- readIORef fired

                    unless (not continuous && name `elem` fs) $ do
                      callback cs
                      writeIORef fired (name:fs)

                fire callback name value rev = do
                    Visibility_ {..} <- ask self
                    VS          {..} <- get self

                    oldcs <- readIORef oldCalculations
                    cs    <- readIORef calculations

                    let matchesDirection  =               value cs /= rev
                        executionPossible = continuous || value cs /= value oldcs

                    when (matchesDirection && executionPossible) (execute callback name)

                    unless once $ modifyIORef fired (Prelude.filter (/= name))

                fireOnPassed = do
                    Visibility_  {..} <- ask self
                    VS           {..} <- get self
                    Calculations {..} <- readIORef calculations

                    for_ onPassed $ \(callback,passed) ->
                        let (thresholdReached,name) =
                                case passed of
                                    PixelsPassed pxs  -> (pixelsPassed     >= pxs      ,toTxt pxs)
                                    PercentPassed per -> (percentagePassed >= per / 100,toTxt per)
                        in thresholdReached # execute (Just callback) name

                handleUpdate = do
                    VS {..} <- get self

                    t       <- readIORef ticking

                    unless t $ do
                        writeIORef ticking True
                        void $ addAnimation update

                -- Animation frames aren't guaranteed to fire before components
                -- are swapped out or removed, when it becomes an error to 
                -- inspect a components environment, state or view. The solution
                -- was a simple catch-all in those cases. It became apparent this
                -- was necessary when using `Visibility` for an infinite scroll 
                -- component where the `Visibility` was swapped out for content.
                update = handle (\(_ :: SomeException) -> pure ()) $ do
                    Visibility_ {..} <- ask self
                    VS          {..} <- get self

                    writeIORef ticking False

                    writeIORef oldCalculations =<< readIORef calculations
                    writeIORef calculations    =<< compute
                    writeIORef verticalOffset  =<< pageYOffset

                    cs <- readIORef calculations

                    for_ onUpdate ($ cs)

                    fireOnPassed

                    let upd rev (callback,name,selector) = fire callback name selector rev

                    traverse_ (upd True)
                        [ (onBottomPassedReverse,"onBottomPassedReverse",bottomPassed)
                        , (onBottomVisibleReverse,"onBottomVisibleReverse",bottomVisible)
                        , (onPassingReverse,"onPassingReverse",passing)
                        , (onTopPassedReverse,"onTopPassedReverse",topPassed)
                        , (onTopVisibleReverse,"onTopVisibleReverse",topVisible)
                        ]

                    traverse_ (upd False)
                        [ (onBottomPassed,"onBottomPassed",bottomPassed)
                        , (onBottomVisible,"onBottomVisible",bottomVisible)
                        , (onPassing,"onPassing",passing)
                        , (onOffScreen,"onOffScreen",offScreen)
                        , (onOnScreen,"onOnScreen",onScreen)
                        , (onTopPassed,"onTopPassed",topPassed)
                        , (onTopVisible,"onTopVisible",topVisible)
                        ]

                compute = do
                    Visibility_ {..} <- ask self
                    VS          {..} <- get self

                    Just r <- readIORef ref

                    BR { brBottom = bottom, brHeight = height, brTop = top, brWidth = width } <- boundingRect (Element r)

                    oldPYO <- readIORef verticalOffset
                    newPYO <- pageYOffset
                    ih     <- fromIntegral <$> innerHeight

                    let (topOffset,bottomOffset) = offset

                        direction    = (newPYO > oldPYO) ? Down $ Up
                        topPassed    = top     < topOffset
                        bottomPassed = bottom  < bottomOffset

                        pixelsPassed     = bottomPassed ? 0 $ max (negate top) 0
                        percentagePassed = pixelsPassed / height

                        bottomVisible = bottom >= bottomOffset && bottom <= ih
                        topVisible    = top    >= topOffset    && top    <= ih

                        fits    = topVisible && bottomVisible
                        passing = topPassed  && not bottomPassed

                        onScreen  = (topVisible || topPassed) && not bottomPassed
                        offScreen = not onScreen

                    return Calculations {..}

            in def
                { construct = VS <$> newIORef def
                                 <*> newIORef def
                                 <*> newIORef def
                                 <*> newIORef def
                                 <*> newIORef def
                                 <*> newIORef def
                                 <*> newIORef def
                , mounted = do
                    Visibility_ {..} <- ask self
                    VS          {..} <- get self
                    for_ context $ \c -> do
                      rh <- onRaw (Node c) "resize" def (\_ _ -> handleUpdate)
                      sh <- onRaw (Node c) "scroll" def (\_ _ -> handleUpdate)
                      writeIORef handlers (VH rh sh)
                    pageYOffset >>= writeIORef verticalOffset
                    when fireOnMount update

                , receive = \newprops oldstate@VS{..} -> do
                    oldprops <- ask self
                    (continuous newprops /= continuous oldprops || once newprops /= once oldprops) #
                        writeIORef fired []
                    return oldstate

                , unmounted = do
                    VS {..} <- get self
                    VH {..} <- readIORef handlers
                    resizeHandler
                    scrollHandler

                , render = \Visibility_ {..} _ ->
                    as (features & Lifecycle (HostRef handleRef)) children

                }

#ifdef __GHCJS__
foreign import javascript unsafe "$r = $1.getBoundingClientRect()" bounding_client_rect_js :: Element -> IO JSV
#endif

data BoundingRect = BR
    { brLeft :: Double
    , brTop :: Double
    , brRight :: Double
    , brBottom :: Double
    , brWidth :: Double
    , brHeight :: Double
    } deriving (Eq)

instance Default BoundingRect where def = BR 0 0 0 0 0 0

boundingRect :: Element -> IO BoundingRect
boundingRect node = do
#ifdef __GHCJS__
  o <- bounding_client_rect_js node
  return $ fromMaybe (error "Semantic.Utils.boundingRect: fromMaybe got Nothing") $ do
    brLeft   <- o .# "left"
    brTop    <- o .# "top"
    brRight  <- o .# "right"
    brBottom <- o .# "bottom"
    brWidth  <- o .# "width"
    brHeight <- o .# "height"
    return BR {..}
#else
    return $ BR 0 0 0 0 0 0
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
    "$r = window.pageYOffset" pageYOffset_js :: IO Int
#endif

pageYOffset :: IO Int
pageYOffset =
#ifdef __GHCJS__
    pageYOffset_js
#else
    return 0
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
    "window.innerHeight" innerHeight_js :: IO Int
#endif

innerHeight :: IO Int
innerHeight =
#ifdef __GHCJS__
    innerHeight_js
#else
    return 0
#endif

data As = As_
pattern As :: HasProp As a => Prop As a -> a -> a
pattern As p a <- (getProp As_ &&& id -> (p,a)) where
    As p a = setProp As_ p a

data Context = Context_
pattern Context :: HasProp Context a => Prop Context a -> a -> a
pattern Context p a <- (getProp Context_ &&& id -> (p,a)) where
    Context p a = setProp Context_ p a

data Continuous = Continuous_
pattern Continuous :: HasProp Continuous a => Prop Continuous a -> a -> a
pattern Continuous p a <- (getProp Continuous_ &&& id -> (p,a)) where
    Continuous p a = setProp Continuous_ p a

data FireOnMount = FireOnMount_
pattern FireOnMount :: HasProp FireOnMount a => Prop FireOnMount a -> a -> a
pattern FireOnMount p a <- (getProp FireOnMount_ &&& id -> (p,a)) where
    FireOnMount p a = setProp FireOnMount_ p a

data OnBottom = OnBottom_
pattern OnBottom :: HasProp OnBottom a => Prop OnBottom a -> a -> a
pattern OnBottom p a <- (getProp OnBottom_ &&& id -> (p,a)) where
    OnBottom p a = setProp OnBottom_ p a

data OnBottomPassed = OnBottomPassed_
pattern OnBottomPassed :: HasProp OnBottomPassed a => Prop OnBottomPassed a -> a -> a
pattern OnBottomPassed p a <- (getProp OnBottomPassed_ &&& id -> (p,a)) where
    OnBottomPassed p a = setProp OnBottomPassed_ p a

data OnBottomPassedReverse = OnBottomPassedReverse_
pattern OnBottomPassedReverse :: HasProp OnBottomPassedReverse a => Prop OnBottomPassedReverse a -> a -> a
pattern OnBottomPassedReverse p a <- (getProp OnBottomPassedReverse_ &&& id -> (p,a)) where
    OnBottomPassedReverse p a = setProp OnBottomPassedReverse_ p a

data OnBottomVisible = OnBottomVisible_
pattern OnBottomVisible :: HasProp OnBottomVisible a => Prop OnBottomVisible a -> a -> a
pattern OnBottomVisible p a <- (getProp OnBottomVisible_ &&& id -> (p,a)) where
    OnBottomVisible p a = setProp OnBottomVisible_ p a

data OnBottomVisibleReverse = OnBottomVisibleReverse_
pattern OnBottomVisibleReverse :: HasProp OnBottomVisibleReverse a => Prop OnBottomVisibleReverse a -> a -> a
pattern OnBottomVisibleReverse p a <- (getProp OnBottomVisibleReverse_ &&& id -> (p,a)) where
    OnBottomVisibleReverse p a = setProp OnBottomVisibleReverse_ p a

data Offset = Offset_
pattern Offset :: HasProp Offset a => Prop Offset a -> a -> a
pattern Offset p a <- (getProp Offset_ &&& id -> (p,a)) where
    Offset p a = setProp Offset_ p a

data Once = Once_
pattern Once :: HasProp Once a => Prop Once a -> a -> a
pattern Once p a <- (getProp Once_ &&& id -> (p,a)) where
    Once p a = setProp Once_ p a

data OnPassed = OnPassed_
pattern OnPassed :: HasProp OnPassed a => Prop OnPassed a -> a -> a
pattern OnPassed p a <- (getProp OnPassed_ &&& id -> (p,a)) where
    OnPassed p a = setProp OnPassed_ p a

data OnPassing = OnPassing_
pattern OnPassing :: HasProp OnPassing a => Prop OnPassing a -> a -> a
pattern OnPassing p a <- (getProp OnPassing_ &&& id -> (p,a)) where
    OnPassing p a = setProp OnPassing_ p a

data OnPassingReverse = OnPassingReverse_
pattern OnPassingReverse :: HasProp OnPassingReverse a => Prop OnPassingReverse a -> a -> a
pattern OnPassingReverse p a <- (getProp OnPassingReverse_ &&& id -> (p,a)) where
    OnPassingReverse p a = setProp OnPassingReverse_ p a

data OnOffScreen = OnOffScreen_
pattern OnOffScreen :: HasProp OnOffScreen a => Prop OnOffScreen a -> a -> a
pattern OnOffScreen p a <- (getProp OnOffScreen_ &&& id -> (p,a)) where
    OnOffScreen p a = setProp OnOffScreen_ p a

data OnOnScreen = OnOnScreen_
pattern OnOnScreen :: HasProp OnOnScreen a => Prop OnOnScreen a -> a -> a
pattern OnOnScreen p a <- (getProp OnOnScreen_ &&& id -> (p,a)) where
    OnOnScreen p a = setProp OnOnScreen_ p a

data OnTopPassed = OnTopPassed_
pattern OnTopPassed :: HasProp OnTopPassed a => Prop OnTopPassed a -> a -> a
pattern OnTopPassed p a <- (getProp OnTopPassed_ &&& id -> (p,a)) where
    OnTopPassed p a = setProp OnTopPassed_ p a

data OnTopPassedReverse = OnTopPassedReverse_
pattern OnTopPassedReverse :: HasProp OnTopPassedReverse a => Prop OnTopPassedReverse a -> a -> a
pattern OnTopPassedReverse p a <- (getProp OnTopPassedReverse_ &&& id -> (p,a)) where
    OnTopPassedReverse p a = setProp OnTopPassedReverse_ p a

data OnTopVisible = OnTopVisible_
pattern OnTopVisible :: HasProp OnTopVisible a => Prop OnTopVisible a -> a -> a
pattern OnTopVisible p a <- (getProp OnTopVisible_ &&& id -> (p,a)) where
    OnTopVisible p a = setProp OnTopVisible_ p a

data OnTopVisibleReverse = OnTopVisibleReverse_
pattern OnTopVisibleReverse :: HasProp OnTopVisibleReverse a => Prop OnTopVisibleReverse a -> a -> a
pattern OnTopVisibleReverse p a <- (getProp OnTopVisibleReverse_ &&& id -> (p,a)) where
    OnTopVisibleReverse p a = setProp OnTopVisibleReverse_ p a

data OnUpdate = OnUpdate_
pattern OnUpdate :: HasProp OnUpdate a => Prop OnUpdate a -> a -> a
pattern OnUpdate p a <- (getProp OnUpdate_ &&& id -> (p,a)) where
    OnUpdate p a = setProp OnUpdate_ p a

instance HasProp As Visibility where
    type Prop As Visibility = Features -> [View] -> View
    getProp _ = as
    setProp _ a v = v { as = a }

instance HasFeatures Visibility where
    getFeatures = features
    setFeatures as v = v { features = as }

instance HasChildren Visibility where
    getChildren = children
    setChildren cs v = v { children = cs }

instance HasProp Context Visibility where
    type Prop Context Visibility = Maybe JSV
    getProp _ = context
    setProp _ c v = v { context = c }

instance HasProp Continuous Visibility where
    type Prop Continuous Visibility = Bool
    getProp _ = continuous
    setProp _ c v = v { continuous = c }

instance HasProp FireOnMount Visibility where
    type Prop FireOnMount Visibility = Bool
    getProp _ = fireOnMount
    setProp _ fom v = v { fireOnMount = fom }

instance HasProp OnBottomPassed Visibility where
    type Prop OnBottomPassed Visibility = Maybe (Calculations -> IO ())
    getProp _ = onBottomPassed
    setProp _ obp v = v { onBottomPassed = obp }

instance HasProp OnBottomPassedReverse Visibility where
    type Prop OnBottomPassedReverse Visibility = Maybe (Calculations -> IO ())
    getProp _ = onBottomPassedReverse
    setProp _ obpr v = v { onBottomPassedReverse = obpr }

instance HasProp OnBottomVisible Visibility where
    type Prop OnBottomVisible Visibility = Maybe (Calculations -> IO ())
    getProp _ = onBottomVisible
    setProp _ obv v = v { onBottomVisible = obv }

instance HasProp OnBottomVisibleReverse Visibility where
    type Prop OnBottomVisibleReverse Visibility = Maybe (Calculations -> IO ())
    getProp _ = onBottomVisibleReverse
    setProp _ obvr v = v { onBottomVisibleReverse = obvr }

instance HasProp Offset Visibility where
    type Prop Offset Visibility = (Double,Double)
    getProp _ = offset
    setProp _ o v = v { offset = o }

instance HasProp Once Visibility where
    type Prop Once Visibility = Bool
    getProp _ = once
    setProp _ o v = v { once = o }

instance HasProp OnPassed Visibility where
    type Prop OnPassed Visibility = [(Calculations -> IO (),Passed)]
    getProp _ = onPassed
    setProp _ op v = v { onPassed = op }

instance HasProp OnPassing Visibility where
    type Prop OnPassing Visibility = Maybe (Calculations -> IO ())
    getProp _ = onPassing
    setProp _ op v = v { onPassing = op }

instance HasProp OnPassingReverse Visibility where
    type Prop OnPassingReverse Visibility = Maybe (Calculations -> IO ())
    getProp _ = onPassingReverse
    setProp _ opr v = v { onPassingReverse = opr }

instance HasProp OnOffScreen Visibility where
    type Prop OnOffScreen Visibility = Maybe (Calculations -> IO ())
    getProp _ = onOffScreen
    setProp _ oos v = v { onOffScreen = oos }

instance HasProp OnOnScreen Visibility where
    type Prop OnOnScreen Visibility = Maybe (Calculations -> IO ())
    getProp _ = onOnScreen
    setProp _ oos v = v { onOnScreen = oos }

instance HasProp OnTopPassed Visibility where
    type Prop OnTopPassed Visibility = Maybe (Calculations -> IO ())
    getProp _ = onTopPassed
    setProp _ otp v = v { onTopPassed = otp }

instance HasProp OnTopPassedReverse Visibility where
    type Prop OnTopPassedReverse Visibility = Maybe (Calculations -> IO ())
    getProp _ = onTopPassedReverse
    setProp _ otpr v = v { onTopPassedReverse = otpr }

instance HasProp OnTopVisible Visibility where
    type Prop OnTopVisible Visibility = Maybe (Calculations -> IO ())
    getProp _ = onTopVisible
    setProp _ otv v = v { onTopVisible = otv }

instance HasProp OnTopVisibleReverse Visibility where
    type Prop OnTopVisibleReverse Visibility = Maybe (Calculations -> IO ())
    getProp _ = onTopVisibleReverse
    setProp _ otvr v = v { onTopVisibleReverse = otvr }

instance HasProp OnUpdate Visibility where
    type Prop OnUpdate Visibility = Maybe (Calculations -> IO ())
    getProp _ = onUpdate
    setProp _ ou v = v { onUpdate = ou }