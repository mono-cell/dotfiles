{-# OPTIONS_GHC -Wno-deprecations #-}

import qualified Codec.Binary.UTF8.String as UTF8
import Control.Arrow (first)
import qualified DBus as D
import qualified DBus.Client as D
import Data.Char (isSpace)
import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.Monoid
import System.Exit (exitSuccess)
import XMonad
import XMonad.Actions.CopyWindow (kill1, killAllOtherCopies)
import XMonad.Actions.CycleWS
  ( WSType(..)
  , moveTo
  , nextScreen
  , prevScreen
  , shiftTo
  )
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotAllDown, rotSlavesDown)
import qualified XMonad.Actions.Search as S
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (killAll, sinkAll)
import XMonad.Hooks.DynamicLog (PP(..), dynamicLogWithPP, wrap)
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
  ( ToggleStruts(..)
  , avoidStruts
  , docks
  , docksEventHook
  )
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName

import           XMonad.Layout.Renamed            (Rename (Replace), renamed)
import           XMonad.Layout.Spacing            (Border (..), spacingRaw)

import XMonad.Layout.WindowArranger (WindowArrangerMsg(..), windowArrange)
import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Input
import XMonad.Prompt.Man
import XMonad.Prompt.Pass
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Ssh
import XMonad.Prompt.XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn)
import XMonad.Util.SpawnOnce

------------------------------------------------------------------------
-- VARIABLES
------------------------------------------------------------------------
-- It's nice to assign values to stuff that you will use more than once
-- in the config. Setting values for things like font, terminal and editor
-- means you only have to change the value here to make changes globally.
myFont :: String
myFont = "xft:Mononoki Nerd Font:bold:size=9"

myModMask :: KeyMask
myModMask = mod4Mask -- Sets modkey to super/windows key

myTerminal :: String
myTerminal = "alacritty" -- Sets default terminal

myBrowser :: String
myBrowser = "firefox"

myEditor :: String
myEditor = "emacsclient -c -a emacs " -- Sets emacs as editor for tree select

-- myEditor = myTerminal ++ " -e vim "    -- Sets vim as editor for tree select
myBorderWidth :: Dimension
myBorderWidth = 2 -- Sets border width for windows

myNormColor :: String
myNormColor = "#2a2a2a" -- Border color of normal windows

myFocusColor :: String
myFocusColor = "#2a2a2a" -- Border color of focused windows

altMask :: KeyMask
altMask = mod1Mask -- Setting this for use in xprompts

-- Colors for polybar
color1, color2, color3, color4 :: String
color1 = "#7F7F7F"

color2 = "#c792ea"

color3 = "#900000"

color4 = "#2E9AFE"

------------------------------------------------------------------------
-- AUTOSTART
------------------------------------------------------------------------
myStartupHook :: X ()
myStartupHook = do
  spawnOnce "~/.config/polybar/launch.sh &"
  setWMName "LG3D"

------------------------------------------------------------------------
-- XPROMPT SETTINGS
------------------------------------------------------------------------
dtXPConfig :: XPConfig
dtXPConfig =
  def
    { font = myFont
    , bgColor = "#292d3e"
    , fgColor = "#d0d0d0"
    , bgHLight = "#c792ea"
    , fgHLight = "#000000"
    , borderColor = "#535974"
    , promptBorderWidth = 2
    , position = CenteredAt {xpCenterY = 0.3, xpWidth = 0.3}
    , height = 40
    , historySize = 256
    , historyFilter = id
    , defaultText = []
    , autoComplete = Just 100000 -- set Just 100000 for .1 sec
    , showCompletionOnTab = False
      -- , searchPredicate     = isPrefixOf
    , searchPredicate = fuzzyMatch
    , alwaysHighlight = True
    , maxComplRows = Nothing -- set to Just 5 for 5 rows
    }

-- The same config above minus the autocomplete feature which is annoying
-- on certain Xprompts, like the search engine prompts.
dtXPConfig' :: XPConfig
dtXPConfig' = dtXPConfig {autoComplete = Nothing}

-- A list of all of the standard Xmonad prompts and a key press assigned to them.
-- These are used in conjunction with keybinding I set later in the config.
promptList :: [(String, XPConfig -> X ())]
promptList =
  [ ("m", manPrompt) -- manpages prompt
  , ("p", passPrompt) -- get passwords (requires 'pass')
  , ("g", passGeneratePrompt) -- generate passwords (requires 'pass')
  , ("r", passRemovePrompt) -- remove passwords (requires 'pass')
  , ("s", sshPrompt) -- ssh prompt
  , ("x", xmonadPrompt) -- xmonad prompt
  ]

-- Same as the above list except this is for my custom prompts.
promptList' :: [(String, XPConfig -> String -> X (), String)]
promptList' =
  [ ("c", calcPrompt, "qalc") -- requires qalculate-gtk
  ]

------------------------------------------------------------------------
-- CUSTOM PROMPTS
------------------------------------------------------------------------
-- calcPrompt requires a cli calculator called qalcualte-gtk.
-- You could use this as a template for other custom prompts that
-- use command line programs that return a single line of output.
calcPrompt :: XPConfig -> String -> X ()
calcPrompt c ans =
  inputPrompt c (trim ans) ?+ \input ->
    liftIO (runProcessWithInput "qalc" [input] "") >>= calcPrompt c
  where
    trim = f . f
      where
        f = reverse . dropWhile isSpace

------------------------------------------------------------------------
-- XPROMPT KEYMAP (emacs-like key bindings for xprompts)
-- Xmonad has several search engines available to use located in
-- XMonad.Actions.Search. Additionally, you can add other search engines
-- such as those listed below.
archwiki, ebay, news, reddit, urban :: S.SearchEngine
archwiki = S.searchEngine "archwiki" "https://wiki.archlinux.org/index.php?search="
ebay = S.searchEngine "ebay" "https://www.ebay.com/sch/i.html?_nkw="
news = S.searchEngine "news" "https://news.google.com/search?q="
reddit = S.searchEngine "reddit" "https://www.reddit.com/search/?q="
urban = S.searchEngine "urban" "https://www.urbandictionary.com/define.php?term="

-- This is the list of search engines that I want to use. Some are from
-- XMonad.Actions.Search, and some are the ones that I added above.
searchList :: [(String, S.SearchEngine)]
searchList =
  [ ("a", archwiki)
  , ("d", S.duckduckgo)
  , ("e", ebay)
  , ("g", S.google)
  , ("h", S.hoogle)
  , ("i", S.images)
  , ("n", news)
  , ("r", reddit)
  , ("s", S.stackage)
  , ("t", S.thesaurus)
  , ("v", S.vocabulary)
  , ("b", S.wayback)
  , ("u", urban)
  , ("w", S.wikipedia)
  , ("y", S.youtube)
  , ("z", S.amazon)
  ]

------------------------------------------------------------------------
-- WORKSPACES
------------------------------------------------------------------------
-- if you are using clickable workspaces. You need the className or title
-- of the program. Use xprop to get this info.
myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook =
  composeAll
    [ className =? "obs" --> doShift ("video.obs")
    , title =? "firefox" --> doShift ("web.browser")
    , title =? "qutebrowser" --> doShift ("web.browser")
    , className =? "mpv" --> doShift ("video.movie player")
    , className =? "vlc" --> doShift ("video.movie player")
    , className =? "Gimp" --> doShift ("graphics.gimp")
    , className =? "Gimp" --> doFloat
    , title =? "Oracle VM VirtualBox Manager" --> doFloat
    , className =? "VirtualBox Manager" --> doShift ("dev.virtualization")
    , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat -- Float Firefox Dialog
    ] <+>
  namedScratchpadManageHook myScratchPads

------------------------------------------------------------------------
-- LOGHOOK
------------------------------------------------------------------------
-- Override the PP values as you would otherwise, adding colors etc depending
-- on the statusbar used
myLogHook :: D.Client -> PP
myLogHook dbus =
  def
    { ppOutput = dbusOutput dbus
    , ppCurrent = wrap ("%{F" ++ color4 ++ "} ") "%{F-}"
    , ppVisible = wrap ("%{F" ++ color1 ++ "} ") "%{F-}"
    , ppUrgent = wrap ("%{F" ++ color3 ++ "} ") "%{F-}"
    , ppHidden = wrap ("%{F" ++ color1 ++ "} ") "%{F-}"
    , ppTitle = wrap ("%{F" ++ color2 ++ "}") "%{F-}"
    , ppSep = "  |  "
    }

-----------------
-- LAYOUT HOOK --
-----------------

myLayout = avoidStruts $ tiled ||| mtiled ||| full
 where
  space = spacingRaw False (Border 5 0 5 0) True (Border 0 5 0 5) True
  rename n = renamed [Replace n]
  tiled = rename "[T]" $ space $ Tall nmaster delta ratio
  mtiled = rename "[T]" $ space $ Mirror $ Tall nmaster delta ratio
  full = rename "[F]" Full
  nmaster = 1
  ratio = 1 / 2
  delta = 3 / 100

------------------------------------------------------------------------
-- SCRATCHPADS
------------------------------------------------------------------------
-- Allows to have several floating scratchpads running different applications.
-- Import Util.NamedScratchpad.  Bind a key to namedScratchpadSpawnAction.
myScratchPads :: [NamedScratchpad]
myScratchPads =
  [ NS "terminal" spawnTerm findTerm manageTerm
  , NS "mocp" spawnMocp findMocp manageMocp
  ]
  where
    spawnTerm = myTerminal ++ " --class scratchpad"
    findTerm = className =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 - h
        l = 0.95 - w
    spawnMocp = myTerminal ++ " -n mocp 'mocp'"
    findMocp = resource =? "mocp"
    manageMocp = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 - h
        l = 0.95 - w

------------------------------------------------------------------------
-- KEYBINDINGS
------------------------------------------------------------------------
myKeys :: [(String, X ())]
myKeys
  -- Xmonad
 =
  [ ("M-C-r", spawn "xmonad --recompile && xmonad --restart") -- Recompiles xmonad
  , ("<KP_Add>", spawn "conf") -- Restarts xmonad
  , ("M-C-q", spawn "polybar-msg cmd restart") -- Restarts xmonad
  , ("M-d d", spawn "rofi -show drun") -- Restarts xmonad
  , ("M-d w", spawn "firefox") -- Restarts xmonad
  , ("M-<Return>", spawn myTerminal)
  , ("M-S-<Return>", shellPrompt dtXPConfig) -- Shell Prompt
  , ("M-q", kill1) -- Kill the currently focused client
  , ("M-t", withFocused $ windows . W.sink) -- Push floating window back to tile
  , ("M-S-t", sinkAll) -- Push ALL floating windows to tile
  , ("<KP_Insert>", namedScratchpadAction myScratchPads "terminal")
  , ("M-C-c", namedScratchpadAction myScratchPads "mocp")
  ] ++
  [("M-s " ++ k, S.promptSearch dtXPConfig' f) | (k, f) <- searchList] ++
  [("M-S-s " ++ k, S.selectSearch f) | (k, f) <- searchList] ++
  [("M-p " ++ k, f dtXPConfig') | (k, f) <- promptList] ++
  [("M-p " ++ k, f dtXPConfig' g) | (k, f, g) <- promptList']
  where
    nonNSP = WSIs (return (\ws -> W.tag ws /= "nsp"))
    nonEmptyNonNSP =
      WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "nsp"))

------------------------------------------------------------------------
-- MAIN
------------------------------------------------------------------------
main :: IO ()
main = do
  dbus <- D.connectSession
  -- Request access to the DBus name
  D.requestName
    dbus
    (D.busName_ "org.xmonad.Log")
    [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  -- The xmonad, ya know...what the window manager is named after.
  xmonad $ ewmh $ docks $ defaults {logHook = dynamicLogWithPP (myLogHook dbus)}

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
  let signal =
        (D.signal objectPath interfaceName memberName)
          {D.signalBody = [D.toVariant $ UTF8.decodeString str]}
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"

defaults =
  def
    { handleEventHook =
        serverModeEventHookCmd <+>
        serverModeEventHook <+>
        serverModeEventHookF "XMONAD_PRINT" (io . putStrLn) <+> docksEventHook
    , modMask = myModMask
    , terminal = myTerminal
    , workspaces = ["ONE", "TWO", "THREE", "FOUR", "FIVE"]
    , layoutHook = myLayout
    , normalBorderColor = myNormColor
    , focusedBorderColor = myFocusColor
    , manageHook = myManageHook <+> manageHook def
    , borderWidth = myBorderWidth
    , startupHook = myStartupHook
    } `additionalKeysP`
  myKeys
