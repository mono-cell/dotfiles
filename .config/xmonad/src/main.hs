-- import System.IO
import           XMonad

import qualified Data.Map                         as M
import qualified XMonad.StackSet                  as W

import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops        (ewmh, ewmhFullscreen)
import           XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks)
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName (setWMName)
import           XMonad.Hooks.StatusBar

import           XMonad.Util.Cursor (setDefaultCursor)
import           XMonad.Util.EZConfig
import           XMonad.Util.Loggers (logTitles)
import           XMonad.Util.NamedScratchpad

import           XMonad.Layout.Renamed            (Rename (Replace), renamed)
import           XMonad.Layout.Spacing            (Border (..), spacingRaw)

import           XMonad.Actions.DynamicWorkspaces

---------------
-- VARIABLES --
---------------

myTerminal :: String
myTerminal = "alacritty"

myBrowser :: String
myBrowser = "firefox"

mySB :: StatusBarConfig
mySB = statusBarProp "~/.local/bin/xmobar" (pure myXmobarPP)


myWorkspaces :: [String]
myWorkspaces = map (\(x, y) -> show x ++ ":" ++ y)
                   (zip [1..5] $ words "TERM WEB FILE WORK MISC")
---------------
-- MAIN HOOK --
---------------

main :: IO ()
main = xmonad . ewmhFullscreen . withSB mySB . docks . ewmh
    $ myConfig

---------------

myConfig =
  def
    { modMask = mod4Mask
    , layoutHook = myLayout
    , manageHook = myManageHook
    , focusFollowsMouse = False
    , startupHook = myStartupHook
    , logHook = myLogHook
    , keys = myKeys
    , borderWidth = 6
    , workspaces    = myWorkspaces
    , focusedBorderColor = "#2a2a2a"
    , normalBorderColor = "#2a2a2a"
    }

-----------------
-- KEYBINDINGS --
-----------------

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys cfg = mkKeymap cfg (myKeymap cfg)

myKeymap :: XConfig Layout -> [(String, X ())]
myKeymap cfg =
     [ ("M-<Return>", spawn myTerminal)
     , ("M-d w", spawn myBrowser)
     , ("M-C-q", spawn "xmonad --recompile && xmonad --restart")
     , ("M-d d", spawn "rofi -show drun")
     , ("M-q", kill)
     , ("M-<Space>", sendMessage NextLayout)
     , ("M-<Left>", windows W.focusUp)
     , ("M-<Right>", windows W.focusDown)
     , ("M-<Tab>", windows W.focusDown)
     , ("S-M-<Left>", windows W.swapUp)
     , ("S-M-<Right>", windows W.swapDown)
     , ("M-S-<Return>", windows W.swapMaster)
     , ("M-t", withFocused $ windows . W.sink)
     , ("<KP_Insert>", namedScratchpadAction myScratchPads "terminal")
     , ("<KP_Add>", namedScratchpadAction myScratchPads "mocp")
     ]
     ++
     zip (map (\x -> fst x++[snd x]) ((zip (repeat "M-") ['1'..'5']))) (map (withNthWorkspace W.greedyView) [0..])
     ++
     zip (map (\x -> fst x++[snd x]) ((zip (repeat "M-S-") ['1'..'5']))) (map (withNthWorkspace W.shift) [0..])

-----------------
-- MANAGE HOOK --
-----------------

myManageHook = composeAll
    [ isFullscreen --> doFullFloat
    , className =? "Gimp" --> doFloat
    , isDialog --> doFloat
    , manageDocks
     ] <+> namedScratchpadManageHook myScratchPads

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

---------------
-- LOG HOOK  --
---------------

myLogHook :: X ()
myLogHook = return()

------------------
-- STARTUP HOOK --
------------------

myStartupHook :: X ()
myStartupHook = do
  setWMName "LG3D"
  setDefaultCursor xC_left_ptr
  checkKeymap def (myKeymap undefined)
  return ()

---------------
-- LOG HOOK  --
---------------

myXmobarPP :: PP
myXmobarPP =
  def
    { ppSep = magenta " • "
    , ppTitleSanitize = xmobarStrip
    , ppCurrent = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent = red . wrap (yellow "!") (yellow "!")
    , ppOrder = \[ws, l, _ ,wins] -> [ws, l, wins]
    , ppExtras = [logTitles formatFocused formatUnfocused]
    }
 where
  formatFocused = wrap (white "[") (white "]") . magenta . ppWindow
  formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue . ppWindow

  ppWindow :: String -> String
  ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

  blue, lowWhite, magenta, red, white, yellow :: String -> String
  magenta = xmobarColor "#ff79c6" ""
  blue = xmobarColor "#bd93f9" ""
  white = xmobarColor "#f8f8f2" ""
  yellow = xmobarColor "#f1fa8c" ""
  red = xmobarColor "#ff5555" ""
  lowWhite = xmobarColor "#696969" ""


------------------------------------------------------------------------
-- SCRATCHPADS
------------------------------------------------------------------------

myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "mocp" spawnMocp findMocp manageMocp
                ]
  where
    spawnTerm  = myTerminal ++ " --class scratchpad"
    findTerm   = resource =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
    spawnMocp  = "alacritty --class mocp -e nvim ~/.config/xmonad/src/main.hs"
    findMocp   = resource =? "mocp"
    manageMocp = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
