{-# LANGUAGE TemplateHaskell #-}

module Main where

import Debug.Trace

import Data.GI.Base (AttrOp((:=)))
import qualified Data.GI.Base as GI
import qualified GI.GLib as GLib
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import qualified GI.Pango as Pango

import Control.Applicative
import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Extra hiding (unit)
import Control.Monad.Trans.Maybe
import Data.Int
import Data.IORef
import Data.Maybe
import Data.Text (Text)
import Data.Time
import Data.Time.Format.ISO8601
import qualified Data.Text as Text
import Database.Persist.Sqlite
import Foreign.Ptr
import Foreign.StablePtr
import Text.URI
import Text.Megaparsec (parseMaybe)
import Unsafe.Coerce

import Format
import Lib

import Cache
import History

data HistEntry = HistEntry
  { _geminiURL :: !GeminiURI
  , _hadjustment :: !Double
  , _vadjustment :: !Double
  }
makeLenses ''HistEntry

data Browser = Browser
  { _history :: !(IORef (History HistEntry))
  , _gtk :: !BrowserGtk
  , _cacheDb :: !SqlBackend
  }

data BrowserGtk = BrowserGtk
  { _window :: !Gtk.Window
  , _backButton :: !Gtk.ToolButton
  , _forwardButton :: !Gtk.ToolButton
  , _refreshButton :: !Gtk.ToolButton
  , _urlEntry :: !Gtk.Entry
  , _textView :: !Gtk.TextView
  , _statusBar :: !Gtk.Statusbar
  }

makeLenses ''Browser
makeLenses ''BrowserGtk

defaultURLs :: [Text]
defaultURLs =
  [ "gemini://gemini.circumlunar.space/docs/faq.gmi"
  , "gemini://gemini.circumlunar.space/docs/specification.gmi"
  ]

main :: IO ()
main = runStderrLoggingT $ withSqliteConn "gemini.db" $ \db -> do
  runReaderT (runMigration migrateAll) db
  
  _gtk <- liftIO $ do
    void $ Gtk.init Nothing

    builder <- Gtk.builderNewFromFile "gemini.ui"

    -- Somehow connecting the signal from glade doesn't work as expected.
    Just winGO <- #getObject builder "window"
    Just windowGtk <- GI.castTo Gtk.Window winGO
    void $ GI.on windowGtk #destroy Gtk.mainQuit

    -- Buttons: no needed actions to bind since Glade, but need their
    -- reference to connect signals while browsing.
    Just backButtonGO <- #getObject builder "back-button"
    Just backButtonGtk <- GI.castTo Gtk.ToolButton backButtonGO
    Just forwardButtonGO <- #getObject builder "forward-button"
    Just forwardButtonGtk <- GI.castTo Gtk.ToolButton forwardButtonGO
    Just refrestButonGO <- #getObject builder "refresh-button"
    Just refreshButtonGtk <- GI.castTo Gtk.ToolButton refrestButonGO

    -- Quit action: destroy main window
    Just accelGroupGO <- #getObject builder "accel-group"
    Just accelGroup <- GI.castTo Gtk.AccelGroup accelGroupGO
    Gtk.accelMapAddEntry "<Gemini>/Quit" Gdk.KEY_q  [Gdk.ModifierTypeControlMask]
    quitClosure <- Gtk.genClosure_AccelGroupActivate $ \_ag _obj _w32 _mods -> do
      #destroy windowGtk
      pure True
    #connectByPath accelGroup "<Gemini>/Quit" quitClosure

    -- Populate a few links in the URL combo.  That's “bookmarks”
    -- until they're implemented proper.
    Just urlCbGO <- #getObject builder "url-cb"
    Just urlCb <- GI.castTo Gtk.ComboBoxText urlCbGO
    mapM_ (#appendText urlCb) defaultURLs

    -- Entry: just to connect signals
    Just urlEntryGO <- #getObject builder "url-entry"
    Just urlEntryGtk <- GI.castTo Gtk.Entry urlEntryGO

    -- URL ComboBoxText: if selection “active” changes from selections, autoactivate
    void $ GI.on urlCb #changed $ do
      newActive <- GI.get urlCb #active
      when (newActive >= 0) $ void $ #activate urlEntryGtk

    -- home page filler
    Just textViewGO <- #getObject builder "body-view"
    Just textViewGtk <- GI.castTo Gtk.TextView textViewGO
    hover <- fmap castStablePtrToPtr . newStablePtr =<< newIORef Nothing
    #setDataFull textViewGtk "hovering" hover (Just (freeStablePtr . castPtrToStablePtr))
    void (GI.on textViewGtk #motionNotifyEvent (motionNotifyEvent textViewGtk))

    -- Statusbar: just to connect signals
    Just statusbarGO <- #getObject builder "statusbar"
    Just statusbarGtk <- GI.castTo Gtk.Statusbar statusbarGO

    -- <pre>: set nowrap (may be buggy in Glade)
    Just preGO <- #getObject builder "pre"
    Just preGtk <- GI.castTo Gtk.TextTag preGO
    GI.set preGtk [ #wrapMode := Gtk.WrapModeNone ]

    -- <li>: set tab array (until I find out whether it's possible in GtkBuilder)
    Just liGO <- #getObject builder "li"
    Just liGtk <- GI.castTo Gtk.TextTag liGO
    liIndent <- GI.get liGtk #indent
    tabs <- Pango.tabArrayNew 1 True
    #setTab tabs 0 Pango.TabAlignLeft (abs liIndent)
    GI.set liGtk [ #tabs := tabs ]

    pure BrowserGtk
      { _window = windowGtk
      , _backButton = backButtonGtk
      , _forwardButton = forwardButtonGtk
      , _refreshButton = refreshButtonGtk
      , _urlEntry = urlEntryGtk
      , _statusBar = statusbarGtk
      , _textView = textViewGtk
      }

  his <- liftIO (newIORef emptyHistory)
  let browser = Browser
        { _history = his
        , _cacheDb = db
        , _gtk = _gtk
        }

  liftIO $ do
    -- URL ComboBoxText entry: when “activated”, load URL
    void $ GI.on (browser ^. gtk . urlEntry) #activate $ do
      urlBuf <- GI.get (browser ^. gtk . urlEntry) #buffer
      urlText <- GI.get urlBuf #text
      case parseMaybe @Text parser urlText >>= validateGeminiURI of
        Just gUri -> followGeminiLink browser gUri
        Nothing -> do
          -- Gtk.messageDialogNew missing from bindings
          dialog <- GI.new Gtk.MessageDialog [ #buttons := Gtk.ButtonsTypeOk
                                            , #destroyWithParent := True
                                            , #messageType := Gtk.MessageTypeError
                                            , #text := "Invalid Gemini URL"
                                            , #transientFor := browser ^. gtk . window
                                            , #useHeaderBar := 0
                                            ]
          void (#run dialog)
          #destroy dialog

    void $ GI.on (browser ^. gtk . backButton) #clicked $ do
      setAdjustments <- adjustmentsFor (browser ^. gtk . textView)
      h <- readIORef (browser ^. history)
      forM_ (moveBack setAdjustments h) (openGeminiLink False browser)

    void $ GI.on (browser ^. gtk . forwardButton) #clicked $ do
      setAdjustments <- adjustmentsFor (browser ^. gtk . textView)
      h <- readIORef (browser ^. history)
      forM_ (moveForward setAdjustments h) (openGeminiLink False browser)

    void $ GI.on (browser ^. gtk . refreshButton) #clicked $
      openGeminiLink True browser =<< readIORef (browser ^. history)

    void $ GI.on (browser ^. gtk . textView) #eventAfter (eventAfter browser)
    void $ GI.on (browser ^. gtk . textView) #queryTooltip (queryTooltip browser)
    void $ GI.on (browser ^. gtk . textView) #populatePopup (populatePopup browser)

    #showAll (browser ^. gtk . window)
    Gtk.main

followGeminiLink :: Browser -> GeminiURI -> IO ()
followGeminiLink browser url = do
  setAdjustments <- adjustmentsFor (browser ^. gtk . textView)
  let newEntry = HistEntry { _geminiURL = url, _hadjustment = 0, _vadjustment = 0 }
  his' <- push setAdjustments newEntry <$> readIORef (browser ^. history)
  openGeminiLink False browser his'

openGeminiLink :: Bool -> Browser -> History HistEntry -> IO ()
openGeminiLink refresh browser his = forM_ (his ^. current) $ \histEntry ->
  void $ forkIO $ do
    let url = histEntry ^. geminiURL
    runReaderT (fetchLink refresh url) browser >>= \case
      Left errMsg -> toGtk $ do
        buf <- #getBuffer (browser ^. gtk . textView)
        GI.set buf [ #text := errMsg ]
        let sb = browser ^. gtk . statusBar
        ctx <- #getContextId sb "page"
        #push sb ctx "Non-2x return code"
      Right (fromCache,entry) -> do
        fc <- if fromCache then do
               delta <- (`diffUTCTime` (entry ^. entryFetchDate)) <$> getCurrentTime
               let s = " (fetched " <> iso8601Show (entry ^. entryFetchDate . to utctDay)
                       <> "—" <> formatDelta delta <> ")"
               pure (Text.pack s)
             else pure mempty
        let st = case entry ^. entrySuccessType of
              SuccessOther n -> Text.pack (show n ++ " ")
              _ -> Text.empty
            sz = Text.pack (" " ++ show (Text.length (entry ^. entryPayload)) ++ " chars")
            status = st <> entry ^. entryMime <> sz <> fc
            doc = parseDoc (entry ^. entryPayload)
        toGtk $ do
          urlBuf <- GI.get (browser ^. gtk . urlEntry) #buffer
          GI.set urlBuf [ #text := render (url ^. _Wrapped) ]
          gmiToGtk url doc (browser ^. gtk . textView)
          void $ GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE $ False <$ restoreHisAdjustments (browser ^. gtk . textView) histEntry
          let sb = browser ^. gtk . statusBar
          ctx <- #getContextId sb "page"
          void (#push sb ctx status)
          updateHisButtons browser his

fetchLink :: Bool -> GeminiURI -> ReaderT Browser IO (Either Text (Bool,Entry))
fetchLink refresh url =
  withReaderT _cacheDb (fetchFromCache url) >>= \case
    Just entry | not refresh -> pure (Right (True,entry))
    _ ->
      liftIO (request url) >>= \case
        Success successType mime payload -> do
          entry <- withReaderT _cacheDb (insertCache url successType mime payload)
          pure (Right (False,entry))
        nonSuccess -> pure (Left (Text.pack (show nonSuccess)))

updateHisButtons :: Browser -> History HistEntry -> IO ()
updateHisButtons browser his = do
  writeIORef (browser ^. history) his
  GI.set (browser ^. gtk . backButton) [ #sensitive := not (null (his ^. back)) ]
  GI.set (browser ^. gtk . forwardButton) [ #sensitive := not (null (his ^. forward)) ]
  GI.set (browser ^. gtk . refreshButton) [ #sensitive := True ]

-- | Schedule an IO action in the GTK thread.
toGtk :: IO a -> IO ()
toGtk a = void $ GLib.idleAdd GLib.PRIORITY_DEFAULT (False <$ a)

-- It's not gmiToPango because links are GtkLinkButtons.
gmiToGtk :: GeminiURI -> Doc -> Gtk.TextView -> IO ()
gmiToGtk base Doc {docLines} tv = do
  buf <- #getBuffer tv
  uncurry (#delete buf) =<< #getBounds buf
  iter <- #getStartIter buf
  let insertLn txt = do
        #insert buf iter txt (-1)
        #insert buf iter "\n" 1

  fromMark <- #createMark buf Nothing iter True
  let tagged tag block = do
        #moveMark buf fromMark iter
        block
        fromIter <- #getIterAtMark buf fromMark
        #applyTagByName buf tag fromIter iter

  forM_ docLines $ \case
    TextLine (SimpleTextLine l) -> insertLn l
    TextLine (HeadingLine lvl h) ->
      let tagName = case lvl of
                      L1 -> "h1"
                      L2 -> "h2"
                      L3 -> "h3"
      in tagged tagName $ insertLn h
    TextLine (ListItem li) -> tagged "li" $ #insert buf iter "•\t" (-1) *> insertLn li
    TextLine (QuoteLine q) -> tagged "q" $ insertLn q
    LinkLine uri mbDesc -> do
      let uriT = render uri
      tagged "href" $ insertLn (fromMaybe uriT mbDesc)
      startIter <- #getIterAtMark buf fromMark
      uriTag <- GI.new Gtk.TextTag []
      tbl <- GI.get buf #tagTable
      void (#add tbl uriTag)
      let Just absUri = uri `relativeTo` unGemini base
      href <- castStablePtrToPtr <$> newStablePtr (absUri :: HRefValue)
      #setDataFull uriTag "href" href (Just (freeStablePtr . castPtrToStablePtr))
      #applyTag buf uriTag startIter iter
    PreBlock _ preBlock -> tagged "pre" $ #insert buf iter (Text.unlines preBlock) (-1)

formatDelta :: NominalDiffTime -> String
formatDelta delta
  | delta < 0 = "IN THE FUTURE!!!"
  | otherwise = show (n :: Int) <> " " <> unit <> s <> " ago"
  where
    (n,unit) | delta <        60 = (secs                  ,"second")
             | delta <      3600 = (secs `div`         60 ,"minute")
             | delta <     86400 = (secs `div`       3600 ,"hour")
             | delta <  30*86400 = (secs `div`      86400 ,"day")
             | delta < 365*86400 = (secs `div`  (30*86400),"month")
             | otherwise =         (secs `div` (365*86400),"year")
    secs = round (nominalDiffTimeToSeconds delta)
    s = if n == 1 then "" else "s"

setCursorIfAppropriate :: Gtk.TextView -> Int32 -> Int32 -> IO ()
setCursorIfAppropriate tv x y = do
  (gotIter,iter) <- #getIterAtLocation tv x y
  hovering <- whenMaybe gotIter $ do
               tags <- Gtk.textIterGetTags iter
               mbUri <- flip firstJustM tags $ \tag -> runMaybeT $ do
                         uriT <- #getData tag "href"
                         guard (uriT /= nullPtr)
                         liftIO (deRefStablePtr (castPtrToStablePtr uriT))
               pure (maybe (Left ()) Right mbUri)
  wasHoveringRef <- deRefStablePtr . castPtrToStablePtr =<< #getData tv "hovering"
  wasHovering <- readIORef wasHoveringRef :: IO (Maybe (Either () URI))
  when (hovering /= wasHovering) $ do
    writeIORef wasHoveringRef hovering
    display <- #getDisplay tv
    handCursor <- Gdk.cursorNewFromName display "pointer"
    regularCursor <- Gdk.cursorNewFromName display "text"
    Just gdkWin <- #getWindow tv Gtk.TextWindowTypeText
    #setCursor gdkWin $ case hovering of
      Nothing -> Nothing
      Just (Right _) -> handCursor
      Just (Left ())  -> regularCursor

motionNotifyEvent :: Gtk.TextView -> Gtk.WidgetMotionNotifyEventCallback
motionNotifyEvent self eventMotion = do
  ex <- round <$> GI.get eventMotion #x
  ey <- round <$> GI.get eventMotion #y
  (x,y) <- #windowToBufferCoords self Gtk.TextWindowTypeWidget ex ey
  setCursorIfAppropriate self x y
  pure False

eventAfter :: Browser -> Gtk.WidgetEventAfterCallback
eventAfter browser event = fmap (fromMaybe ()) $ runMaybeT $ do
  et <- GI.get event #type
  (ex,ey) <- case et of
              Gdk.EventTypeButtonRelease -> do
                let event' = unsafeCoerce event :: Gdk.EventButton
                button <- GI.get event' #button
                if fromIntegral button /= Gdk.BUTTON_PRIMARY
                  then empty
                  else do
                    ex <- round <$> GI.get event' #x
                    ey <- round <$> GI.get event' #y
                    pure (ex,ey)
              Gdk.EventTypeTouchEnd -> do
                let event' = unsafeCoerce event :: Gdk.EventTouch
                ex <- round <$> GI.get event' #x
                ey <- round <$> GI.get event' #y
                pure (ex,ey)
              _ -> empty
  let tv = browser ^. gtk . textView
  buffer <- GI.get tv #buffer
  hasSelection <- GI.get buffer #hasSelection
  guard (not hasSelection)

  (x,y) <- #windowToBufferCoords tv Gtk.TextWindowTypeWidget ex ey
  (gotIter,iter) <- #getIterAtLocation tv x y
  when gotIter $ followIfLink browser iter

type HRefValue = URI

followIfLink :: MonadIO m => Browser -> Gtk.TextIter -> m ()
followIfLink browser iter = do
  tags <- #getTags iter
  mbUri <- flip firstJustM tags $ \tag -> runMaybeT $ do
    uriT <- #getData tag "href"
    guard (uriT /= nullPtr)
    liftIO (deRefStablePtr (castPtrToStablePtr uriT))
  mapM_ (openLink browser) mbUri

openLink :: MonadIO m => Browser -> URI -> m ()
openLink browser uri = case validateGeminiURI uri of
  Just guri -> liftIO (followGeminiLink browser guri)
  Nothing -> do
    let win = browser ^. gtk . window
    Gtk.showUriOnWindow (Just win) (render uri) (fromIntegral Gdk.CURRENT_TIME)

queryTooltip :: Browser -> Gtk.WidgetQueryTooltipCallback
queryTooltip _ _ _ True _ = traceM "keyboard mode tooltip" *> pure False
queryTooltip browser ex ey False tooltip = fmap (maybe False (const True)) $ runMaybeT $ do
  let tv = browser ^. gtk . textView
  (x,y) <- #windowToBufferCoords tv Gtk.TextWindowTypeWidget ex ey
  (gotIter,iter) <- #getIterAtLocation tv x y
  guard gotIter
  tags <- #getTags iter
  uri <- MaybeT $ flip firstJustM tags $ \tag -> runMaybeT $ do
    uriT <- #getData tag "href"
    guard (uriT /= nullPtr)
    liftIO (deRefStablePtr (castPtrToStablePtr uriT))
  Gtk.tooltipSetText tooltip (Just (render uri))

populatePopup :: Browser -> Gtk.TextViewPopulatePopupCallback
populatePopup browser widget = do
  hoveringPtr <- #getData (browser ^. gtk . textView) "hovering"
  guard (hoveringPtr /= nullPtr)
  mbHovering <- readIORef =<< deRefStablePtr (castPtrToStablePtr hoveringPtr)
  case mbHovering of
    Just (Right url) -> do
      Just menu <- GI.castTo Gtk.Menu widget
      menuItemCopy <- Gtk.menuItemNewWithMnemonic "_Copy URL"
      void $ GI.on menuItemCopy #activate $ copyActivate browser (render url)
      #show menuItemCopy
      #append menu menuItemCopy
      menuItemOpen <- Gtk.menuItemNewWithMnemonic "_Open URL"
      void $ GI.on menuItemOpen #activate $ openLink browser url
      #show menuItemOpen
      #append menu menuItemOpen
    _ -> pure ()

copyActivate :: Browser -> Text -> Gtk.MenuItemActivateCallback
copyActivate browser uri = do
  clipboardAtom <- Gdk.atomIntern "CLIPBOARD" False
  clipboard <- #getClipboard (browser ^. gtk . textView) clipboardAtom
  #setText clipboard uri (-1)

adjustmentsFor :: Gtk.TextView -> IO (HistEntry -> HistEntry)
adjustmentsFor tv = do
  hadj <- GI.get tv #hadjustment >>= \a -> GI.get a #value
  vadj <- GI.get tv #vadjustment >>= \a -> GI.get a #value
  pure (set vadjustment vadj . set hadjustment hadj)

restoreHisAdjustments :: Gtk.TextView -> HistEntry -> IO ()
restoreHisAdjustments tv histEntry = do
  hadj <- GI.get tv #hadjustment
  GI.set hadj [ #value := histEntry ^. hadjustment ]
  vadj <- GI.get tv #vadjustment
  GI.set vadj [ #value := histEntry ^. vadjustment ]
