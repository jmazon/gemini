{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.GI.Base (AttrOp((:=)))
import qualified Data.GI.Base as GI
import qualified GI.GLib as GLib
import qualified GI.GObject as GO
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk

import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Foldable
import Data.IORef
import Data.Text (Text)
import Data.Time
import Data.Time.Format.ISO8601
import qualified Data.Text as Text
import Database.Persist.Sqlite
import Text.URI
import Text.Megaparsec (parseMaybe)

import Format
import Lib

import Cache
import History
import LoremIpsum

data Browser = Browser
  { _history :: !(IORef History)
  , _gtk :: !BrowserGtk
  , _cacheDb :: !SqlBackend
  }

data BrowserGtk = BrowserGtk
  { _window :: !Gtk.Window
  , _backButton :: !Gtk.ToolButton
  , _fwdButton :: !Gtk.ToolButton
  , _refreshButton :: !Gtk.ToolButton
  , _urlEntry :: !Gtk.Entry
  , _textContent :: !Gtk.TextView
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

    win <- GI.new Gtk.Window [ #title := "Gemini" ]
    void $ GI.on win #destroy Gtk.mainQuit

    ag <- GI.new Gtk.AccelGroup []
    Gtk.accelMapAddEntry "<Gemini>/Back"      Gdk.KEY_b [Gdk.ModifierTypeControlMask]
    Gtk.accelMapAddEntry "<Gemini>/Forward"   Gdk.KEY_f [Gdk.ModifierTypeControlMask]
    Gtk.accelMapAddEntry "<Gemini>/Refresh"   Gdk.KEY_r [Gdk.ModifierTypeControlMask]
    Gtk.accelMapAddEntry "<Gemini>/Focus-URL" Gdk.KEY_l [Gdk.ModifierTypeControlMask]
    Gtk.accelMapAddEntry "<Gemini>/Quit"      Gdk.KEY_q [Gdk.ModifierTypeControlMask]
    #addAccelGroup win ag

    quitClosure <- Gtk.genClosure_AccelGroupActivate $ \_ag _obj _w32 _mods -> do
      #destroy win
      pure True
    #connectByPath ag "<Gemini>/Quit" quitClosure

    box <- GI.new Gtk.Box [ #orientation := Gtk.OrientationVertical ]
    #add win box

    toolbar <- GI.new Gtk.Toolbar []
    #add box toolbar

    backBtn <- GI.new Gtk.ToolButton [ #label := "Back", #iconName := "go-previous", #sensitive := False ]
    backBtnGValue <- GI.toGValue (Just backBtn)
    toolButtonGType <- Gtk.glibType @Gtk.ToolButton
    (_,toolButtonClickedSignal,tbcDetail) <- GO.signalParseName "clicked" toolButtonGType False
    backClosure <- Gtk.genClosure_AccelGroupActivate $ \_ag _obj _word32 _mods -> do
      void (GO.signalEmitv [backBtnGValue] toolButtonClickedSignal tbcDetail)
      pure True
    #connectByPath ag "<Gemini>/Back" backClosure
    #add toolbar backBtn

    fwdBtn <- GI.new Gtk.ToolButton [ #label := "Forward", #iconName := "go-next", #sensitive := False ]
    forwardBtnGValue <- GI.toGValue (Just fwdBtn)
    forwardClosure <- Gtk.genClosure_AccelGroupActivate $ \_ag _obj _word32 _mods -> do
      void (GO.signalEmitv [forwardBtnGValue] toolButtonClickedSignal 0)
      pure True
    #connectByPath ag "<Gemini>/Forward" forwardClosure
    #add toolbar fwdBtn

    refreshBtn <- GI.new Gtk.ToolButton [ #label := "Refresh", #iconName := "view-refresh", #sensitive := False ]
    #add toolbar refreshBtn

    refreshBtnGValue <- GI.toGValue (Just refreshBtn)
    refreshClosure <- Gtk.genClosure_AccelGroupActivate $ \_ag _obj _word32 _mods -> do
      void (GO.signalEmitv [refreshBtnGValue] toolButtonClickedSignal 0)
      pure True
    #connectByPath ag "<Gemini>/Refresh" refreshClosure

    urlTi <- GI.new Gtk.ToolItem []
    #add toolbar urlTi
    urlTiBox <- GI.new Gtk.Box []
    #add urlTi urlTiBox
    urlLbl <- Gtk.labelNewWithMnemonic (Just "_URL")
    #add urlTiBox urlLbl
    urlCb <- Gtk.comboBoxTextNewWithEntry
    GI.set urlCb [ #hexpand := True ]
    mapM_ (#appendText urlCb) defaultURLs
    Just entryWdg <- #getChild urlCb
    Just urlEntryGtk <- GI.castTo Gtk.Entry entryWdg
    GI.set urlEntryGtk [ #maxWidthChars := -1 ]
    #setMnemonicWidget urlLbl (Just urlCb)
    focusUrlClosure <- Gtk.genClosure_AccelGroupActivate $ \_ag _obj _w32 _mods -> do
      #grabFocus urlCb
      pure True
    #connectByPath ag "<Gemini>/Focus-URL" focusUrlClosure
    #add urlTiBox urlCb
    -- URL ComboBoxText: if selection “active” changes from selections,
    -- autoactivate
    void $ GI.on urlCb #changed $ do
      newActive <- GI.get urlCb #active
      when (newActive >= 0) $ void $ #activate urlEntryGtk

    sw <- GI.new Gtk.ScrolledWindow [ #vexpand := True ]
    #add box sw
    textView <- GI.new Gtk.TextView
      [ #cursorVisible := False
      , #editable := False
      , #wrapMode := Gtk.WrapModeWord
      ]
    #add sw textView
    buf <- #getBuffer textView
    GI.set buf [ #text := loremIpsum ]

    statusBarGtk <- GI.new Gtk.Statusbar []
    #add box statusBarGtk

    pure BrowserGtk
      { _window = win
      , _backButton = backBtn
      , _fwdButton = fwdBtn
      , _refreshButton = refreshBtn
      , _urlEntry = urlEntryGtk
      , _statusBar = statusBarGtk
      , _textContent = textView
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
        Just gUri -> followLink browser gUri
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
      h <- readIORef (browser ^. history)
      forM_ (moveBack h) (openLink False browser)

    void $ GI.on (browser ^. gtk . fwdButton) #clicked $ do
      h <- readIORef (browser ^. history)
      forM_ (moveForward h) (openLink False browser)

    void $ GI.on (browser ^. gtk . refreshButton) #clicked $
      openLink True browser =<< readIORef (browser ^. history)

    #showAll (browser ^. gtk . window)
    Gtk.main

followLink :: Browser -> GeminiURI -> IO ()
followLink browser url = do
  his' <- push url <$> readIORef (browser ^. history)
  openLink False browser his'

openLink :: Bool -> Browser -> History -> IO ()
openLink refresh browser his = forM_ (his ^. current) $ \url ->
  void $ forkIO $ do
    runReaderT (fetchLink refresh url) browser >>= \case
      Left errMsg -> toGtk $ do
        buf <- #getBuffer (browser ^. gtk . textContent)
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
          gmiToGtk url doc (browser ^. gtk . textContent) (followLink browser)
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

updateHisButtons :: Browser -> History -> IO ()
updateHisButtons browser his = do
  writeIORef (browser ^. history) his
  GI.set (browser ^. gtk . backButton) [ #sensitive := not (null (his ^. back)) ]
  GI.set (browser ^. gtk . fwdButton) [ #sensitive := not (null (his ^. forward)) ]
  GI.set (browser ^. gtk . refreshButton) [ #sensitive := True ]

-- | Schedule an IO action in the GTK thread.
toGtk :: IO a -> IO ()
toGtk a = void $ Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT (False <$ a)

-- It's not gmiToPango because links are GtkLinkButtons.
gmiToGtk :: GeminiURI -> Doc -> Gtk.TextView -> (GeminiURI -> IO ()) -> IO ()
gmiToGtk base Doc {docLines} tv follow = do
  buf <- #getBuffer tv
  uncurry (#delete buf) =<< #getBounds buf
  iter <- #getStartIter buf
  forM_ docLines $ \case
    TextLine (SimpleTextLine l) -> #insert buf iter (l <> "\n") (-1)
    TextLine (HeadingLine lvl h) -> do
      let big = 2 - fromEnum lvl
      esc <- GLib.markupEscapeText h (-1)
      #insertMarkup buf iter
        (fold (replicate big "<big>") <> "<b>"
         <> esc
         <> "</b>" <> fold (replicate big "</big>")
         <> "\n")
        (-1)
    TextLine (ListItem li) -> #insert buf iter ("\t• " <> li <> "\n") (-1)
    TextLine (QuoteLine q) -> do
      esc <- GLib.markupEscapeText q (-1)
      #insertMarkup buf iter ("<i>" <> esc <> "</i>\n") (-1)
    LinkLine uri mbDesc -> do
      anchor <- #createChildAnchor buf iter
      #insert buf iter "\n" 1
      let uriT = render uri
      link <- Gtk.linkButtonNewWithLabel uriT mbDesc
      case validateGeminiURI =<< uri `relativeTo` unGemini base of
        Just gUri -> void $ GI.on link #activateLink $ True <$ follow gUri
        Nothing -> pure ()
      #show link
      #addChildAtAnchor tv link anchor
    PreBlock _ preBlock -> do
      esc <- GLib.markupEscapeText (Text.unlines preBlock) (-1)
      #insertMarkup buf iter ("<tt>" <> esc <> "</tt>") (-1)

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
