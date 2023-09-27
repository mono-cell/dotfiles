module Main (main) where

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import           Control.Monad                      ((>=>))

import qualified Data.Map                           as M
import           Data.Monoid
import qualified Data.Text                          as T

import           System.Exit                        (exitSuccess)
import           System.Environment

import           XMonad
import qualified XMonad.Prompt                      as P
import qualified XMonad.StackSet                    as W

import           XMonad.Actions.CopyWindow          (copy)
import           XMonad.Actions.CycleWS
import           XMonad.Actions.DynamicWorkspaces
import           XMonad.Actions.GridSelect
import           XMonad.Actions.Minimize
import           XMonad.Actions.MouseResize         (mouseResize)
import qualified XMonad.Actions.Search              as S
import           XMonad.Actions.Sift
-- import           XMonad.Actions.TopicSpace          as TS
import           XMonad.Actions.SpawnOn
import           XMonad.Actions.WindowBringer

import           XMonad.Hooks.DynamicIcons
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks

import           XMonad.Hooks.StatusBar
import           XMonad.Hooks.StatusBar.PP
import           XMonad.Hooks.ManageHelpers (isDialog)
import           XMonad.Hooks.WindowSwallowing
import           XMonad.Hooks.WorkspaceHistory

import           XMonad.Layout.Fullscreen
import           XMonad.Layout.LayoutCombinators    (JumpToLayout (..))
import qualified XMonad.Layout.MultiToggle          as MT
import           XMonad.Layout.SubLayouts
import qualified XMonad.Layout.ToggleLayouts        as T
import           XMonad.Layout.WindowArranger
import           XMonad.Layout.WindowNavigation


import           XMonad.Util.Dzen                   as DZ
import           XMonad.Util.Dmenu                  as D
import           XMonad.Util.EZConfig
import           XMonad.Util.Paste                  (pasteSelection)
import           XMonad.Util.Run                    (runInTerm)
import           XMonad.Util.SpawnOnce              (spawnOnce)
import           XMonad.Util.Ungrab                 (unGrab)
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.WorkspaceCompare

import           XMonad.Prompt.XMonad


xmPP :: PP
xmPP =
    def
      { ppCurrent = xmobarColor "white" "" . wrap "[" "]"
      , ppVisible = wrap "<" ">"
      , ppUrgent  = xmobarColor "red"   "" . wrap "!" "!"
      , ppTitle   = shorten 80
      , ppLayout  = id
      , ppExtras  = []
      , ppSort    = getSortByIndex
      -- , ppHidden  = id
      -- , ppHiddenNoWindows = const ""
      -- , ppVisibleNoWindows = Nothing
      -- , ppPrinters = empty
      }

xmIcons :: Query [String]
xmIcons = composeAll
  [ className =? "Firefox"  <||> className =? "firefox"   --> appIcon "\xE745"
  , className =? "Chromium" <||> className =? "chromium"  --> appIcon "\xE743"
  , className =? "Spotify"  <||> className =? "spotify"   --> appIcon "\xF1BC"
  , className =? "kitty"    <||> className =? "alacritty" --> appIcon "\xE795"
  ]


xmobarPrimary = let cmd = home ++ "/.local/bin/xmobar-0"
                 in statusBarPropTo "_XMONAD_LOG_0" cmd (pure xmPP)

xmobarAlternate = "/.local/bin/xmobar-1" statusBarPropTo "_XMONAD_LOG_1" cmd (pure xmPP)

xmobar :: ScreenId -> StatusBarConfig
xmobar sc = let cmd  = home ++ "/.local/bin/xmobar-" ++ show sc
                log  = "_XMONAD_LOG_" ++ show sc
             in statusBarPropTo log cmd (pure xmPP)

------------------------------------------------------------------------------

main :: IO()
main =
  xmonad
    . ewmh
    . ewmhFullscreen
    . withSB (xmobarPrimary  <> xmobarAlternate)
    . docks
    $ def
    { modMask = mod4Mask
    , layoutHook = myLayout
    , manageHook = myManageHook
    , focusFollowsMouse = False
    , startupHook = myStartupHook
    , logHook = myLogHook
    , workspaces = [ "1", "2", "3", "4", "5" ]
    , focusedBorderColor = "#2a2a2a"
    , normalBorderColor = "#2a2a2a"
    , keys = myKeys
    , borderWidth = 6
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
     zip (map (\x -> fst x++[snd x]) ((zip (repeat "M-") (['1'..'5'])))) (map (withNthWorkspace W.greedyView) [0..])
     +
     zip (map (\x -> fst x++[snd x]) ((zip (repeat "M-S-") (['1'..'5'])))) (map (withNthWorkspace W.shift) [0..])

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


