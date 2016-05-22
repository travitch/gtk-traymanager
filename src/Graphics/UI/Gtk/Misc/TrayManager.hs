{-# LANGUAGE ScopedTypeVariables, CPP, OverloadedStrings, NegativeLiterals, ConstraintKinds, TypeFamilies, MultiParamTypeClasses, KindSignatures, FlexibleInstances, UndecidableInstances, DataKinds, FlexibleContexts #-}
-- | This module implements a TrayManager - an integral part of a
-- Linux system tray widget, though it is not itself a widget.  This
-- package exports a single GObject (for use with gtk2hs) that
-- implements the freedesktop.org system tray specification (it
-- handles receiving events and translating them into convenient
-- signals, along with the messy work of dealing with XEMBED).
--
-- The basic usage of the object is to:
--
-- 1. Instantiate the object with 'trayManagerNew'
--
-- 2. Have it manage a specific screen with 'trayManagerManageScreen'
--
-- 3. Set up handlers for the events exposed by the tray (e.g., 'trayIconAdded').
--
-- As an example, a functional system tray widget looks something like:
--
-- > import GI.Gdk.Objects.Display
-- > import GI.Gtk
-- > import Graphics.UI.Gtk.Misc.TrayManager
-- >
-- > systrayNew = do
-- >     box <- boxNew OrientationHorizontal 5
-- >
-- >     trayManager <- trayManagerNew
-- >     Just disp <- displayGetDefault
-- >     screen <- displayGetScreen disp 0
-- >
-- >     trayManagerManageScreen trayManager screen
-- >
-- >     onTrayIconAdded trayManager $ \w -> do
-- >         widgetShowAll w
-- >         boxPackStart box w False False 0
-- >
-- >     widgetSetSizeRequest box (-1) 25  -- bar height
-- >
-- >     widgetShowAll box
-- >     toWidget box

-- Note that the widgets made available in the event handlers are not
-- shown by default; you need to explicitly show them if you want that
-- (and you probably do).
module Graphics.UI.Gtk.Misc.TrayManager (
    -- * Types
    TrayManager,
    TrayManagerK,
    toTrayManager,
    noTrayManager,

    TrayManagerChild,

    -- * Functions
    trayManagerCheckRunning,
    trayManagerNew,
    trayManagerManageScreen,
    trayManagerGetChildTitle,

    -- * Signals
    TrayIconAddedCallback,
    TrayIconAddedCallbackC,
    TrayIconAddedSignalInfo,
    afterTrayIconAdded,
    trayIconAddedCallbackWrapper,
    trayIconAddedClosure,
    mkTrayIconAddedCallback,
    noTrayIconAddedCallback,
    onTrayIconAdded,

    TrayIconRemovedCallback,
    TrayIconRemovedCallbackC,
    TrayIconRemovedSignalInfo,
    afterTrayIconRemoved,
    trayIconRemovedCallbackWrapper,
    trayIconRemovedClosure,
    mkTrayIconRemovedCallback,
    noTrayIconRemovedCallback,
    onTrayIconRemoved,
  ) where

import Prelude ()
import Data.GI.Base.ShortPrelude

import qualified Data.Text as T

import qualified GI.GObject as GObject

import GI.Gdk.Objects.Screen

import GI.Gtk

import Foreign
import Foreign.C.Types
import Unsafe.Coerce ( unsafeCoerce )

newtype TrayManager = TrayManager (ForeignPtr TrayManager)
foreign import ccall "egg_tray_manager_get_type"
  c_egg_tray_manager_get_type :: IO GType

type instance ParentTypes TrayManager = TrayManagerParentTypes
type TrayManagerParentTypes = '[Widget, GObject.Object]

instance GObject TrayManager where
    gobjectIsInitiallyUnowned _ = True
    gobjectType _ = c_egg_tray_manager_get_type

class GObject o => TrayManagerK o
instance (GObject o, IsDescendantOf TrayManager o) => TrayManagerK o

toTrayManager :: TrayManagerK o => o -> IO TrayManager
toTrayManager = unsafeCastTo TrayManager

noTrayManager :: Maybe TrayManager
noTrayManager = Nothing

foreign import ccall "egg_tray_manager_new"
  egg_tray_manager_new :: IO (Ptr TrayManager)

trayManagerNew :: MonadIO m => m TrayManager
trayManagerNew = liftIO $ do
    result <- egg_tray_manager_new
    checkUnexpectedReturnNULL "egg_tray_manager_new" result
    result' <- (newObject TrayManager) result
    return result'

--------------------

type TrayManagerChild = Ptr EggTrayManagerChild

-- Empty data tags to classify some foreign pointers
data EggTrayManagerChild

foreign import ccall "egg_tray_manager_check_running"
  c_egg_tray_manager_check_running :: Ptr Screen -> IO CInt

trayManagerCheckRunning :: MonadIO m => Screen -> m Bool
trayManagerCheckRunning gdkScreen = liftIO $ do
  let ptrScreen = unsafeCoerce gdkScreen :: ForeignPtr Screen
  withForeignPtr ptrScreen $ \realPtr -> do
    res <- c_egg_tray_manager_check_running realPtr
    return (res /= 0)

foreign import ccall "egg_tray_manager_manage_screen"
  c_egg_tray_manager_manage_screen :: Ptr TrayManager -> Ptr Screen -> IO CInt

trayManagerManageScreen ::
    (MonadIO m, TrayManagerK a) =>
    a
    -> Screen
    -> m Bool
trayManagerManageScreen trayManager screen = liftIO $ do
  let ptrManager = unsafeCoerce trayManager :: ForeignPtr TrayManager
      ptrScreen = unsafeCoerce screen :: ForeignPtr Screen
  res <- withForeignPtr ptrManager $ \realManager -> do
    withForeignPtr ptrScreen $ \realScreen -> do
      c_egg_tray_manager_manage_screen realManager realScreen
  return (res /= 0)

foreign import ccall "egg_tray_manager_get_child_title"
  c_egg_tray_manager_get_child_title :: Ptr TrayManager -> Ptr EggTrayManagerChild -> IO (Ptr CChar)


trayManagerGetChildTitle :: TrayManager -> TrayManagerChild -> IO T.Text
trayManagerGetChildTitle trayManager child = do
  let ptrManager = unsafeCoerce trayManager :: ForeignPtr TrayManager
  res <- withForeignPtr ptrManager $ \realManager -> do
    c_egg_tray_manager_get_child_title realManager child
  cstringToText res

-- | The signal emitted when a new tray icon is added.  These are
-- delivered even for systray icons that already exist when the tray
-- manager is created.

data TrayIconAddedSignalInfo
instance SignalInfo TrayIconAddedSignalInfo where
    type HaskellCallbackType TrayIconAddedSignalInfo = TrayIconAddedCallback
    connectSignal _ = connectTrayIconAdded

type TrayIconAddedCallback = Widget -> IO ()

noTrayIconAddedCallback :: Maybe TrayIconAddedCallback
noTrayIconAddedCallback = Nothing

type TrayIconAddedCallbackC =
    Ptr () ->
    Ptr Widget ->
    Ptr () ->
    IO ()

foreign import ccall "wrapper"
    mkTrayIconAddedCallback :: TrayIconAddedCallbackC -> IO (FunPtr TrayIconAddedCallbackC)

trayIconAddedClosure :: TrayIconAddedCallback -> IO Closure
trayIconAddedClosure cb = newCClosure =<< mkTrayIconAddedCallback wrapped
    where wrapped = trayIconAddedCallbackWrapper cb

trayIconAddedCallbackWrapper ::
    TrayIconAddedCallback ->
    Ptr () ->
    Ptr Widget ->
    Ptr () ->
    IO ()
trayIconAddedCallbackWrapper _cb _ object _ = do
    object' <- (newObject Widget) object
    _cb  object'

onTrayIconAdded :: (GObject a, MonadIO m) => a -> TrayIconAddedCallback -> m SignalHandlerId
onTrayIconAdded obj cb = liftIO $ connectTrayIconAdded obj cb SignalConnectBefore
afterTrayIconAdded :: (GObject a, MonadIO m) => a -> TrayIconAddedCallback -> m SignalHandlerId
afterTrayIconAdded obj cb = connectTrayIconAdded obj cb SignalConnectAfter

connectTrayIconAdded :: (GObject a, MonadIO m) =>
                         a -> TrayIconAddedCallback -> SignalConnectMode -> m SignalHandlerId
connectTrayIconAdded obj cb after_ = liftIO $ do
    cb' <- mkTrayIconAddedCallback (trayIconAddedCallbackWrapper cb)
    connectSignalFunPtr obj "tray_icon_added" cb' after_

-- | This signal is emitted when a tray icon is removed by its parent
-- application.  No action is really necessary here (the icon is
-- removed without any intervention).  You could do something here if
-- you wanted, though.

data TrayIconRemovedSignalInfo
instance SignalInfo TrayIconRemovedSignalInfo where
    type HaskellCallbackType TrayIconRemovedSignalInfo = TrayIconRemovedCallback
    connectSignal _ = connectTrayIconRemoved

type TrayIconRemovedCallback = Widget -> IO ()

noTrayIconRemovedCallback :: Maybe TrayIconRemovedCallback
noTrayIconRemovedCallback = Nothing

type TrayIconRemovedCallbackC =
    Ptr () ->
    Ptr Widget ->
    Ptr () ->
    IO ()

foreign import ccall "wrapper"
    mkTrayIconRemovedCallback :: TrayIconRemovedCallbackC -> IO (FunPtr TrayIconRemovedCallbackC)

trayIconRemovedClosure :: TrayIconRemovedCallback -> IO Closure
trayIconRemovedClosure cb = newCClosure =<< mkTrayIconRemovedCallback wrapped
    where wrapped = trayIconRemovedCallbackWrapper cb

trayIconRemovedCallbackWrapper ::
    TrayIconRemovedCallback ->
    Ptr () ->
    Ptr Widget ->
    Ptr () ->
    IO ()
trayIconRemovedCallbackWrapper _cb _ object _ = do
    object' <- (newObject Widget) object
    _cb  object'

onTrayIconRemoved :: (GObject a, MonadIO m) => a -> TrayIconRemovedCallback -> m SignalHandlerId
onTrayIconRemoved obj cb = liftIO $ connectTrayIconRemoved obj cb SignalConnectBefore
afterTrayIconRemoved :: (GObject a, MonadIO m) => a -> TrayIconRemovedCallback -> m SignalHandlerId
afterTrayIconRemoved obj cb = connectTrayIconRemoved obj cb SignalConnectAfter

connectTrayIconRemoved :: (GObject a, MonadIO m) =>
                         a -> TrayIconRemovedCallback -> SignalConnectMode -> m SignalHandlerId
connectTrayIconRemoved obj cb after_ = liftIO $ do
    cb' <- mkTrayIconRemovedCallback (trayIconRemovedCallbackWrapper cb)
    connectSignalFunPtr obj "tray_icon_added" cb' after_
