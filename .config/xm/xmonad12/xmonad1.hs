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

import           XMonad.Hooks.Modal
import           XMonad.Hooks.StatusBar
import           XMonad.Hooks.StatusBar.PP
import           XMonad.Hooks.ManageHelpers (isDialog)
import           XMonad.Hooks.WindowSwallowing
import           XMonad.Hooks.WorkspaceHistory

import qualified XMonad.Layout.BinarySpacePartition as BSP
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.LayoutCombinators    (JumpToLayout (..))
import           XMonad.Layout.Magnifier
import           XMonad.Layout.Maximize
import           XMonad.Layout.Minimize
import qualified XMonad.Layout.MultiToggle          as MT
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Spiral
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

-- import XMonad.Hooks.RefocusLast
-- import XMonad.Actions.CycleWorkspaceByScreen
-- import XMonad.Layout.Mosaic

-- TODO: dmenu
-- TODO: Advanced layuouts config
-- TODO: XMonad.contrib modal
-- TODO: more workspace and monitor control
-- TODO: scratchpad workspace
-- TODO: xmobar pretty print & xmonad log hooks
-- TODO: XMonad layout internals
-- TODO: greedy view and view?

type Cell    = (String, String)
type Grid    = [Cell]
type KeyPair = (KeyMask, KeySym)
type KeyMap  = (KeyPair, X ())

runInTerm' :: String ->X()
runInTerm' = runInTerm ""

runInBash :: String -> X()
runInBash prog = runInTerm' ("bash -is eval " ++ prog)

dz :: String -> X ()
dz = DZ.dzenConfig (timeout 10 >=> onCurr xScreen)

dm :: MonadIO m => [String] -> [String] -> m String
dm args = D.menuArgs "dmenu" (["-b", "-h", "20"] ++ args)


dmenuWS :: X WorkspaceId
dmenuWS = getTagSortedWS >>= dm args
    where
        args :: [String]
        args = ["-p", "Which workspace, master?"]

getTagSortedWS :: X [String]
getTagSortedWS = do
    ws   <- gets (W.workspaces . windowset)
    sort <- getSortByIndex
    return (map W.tag $ sort ws)


selectWorkspace' :: X ()
selectWorkspace' = do
    s <- gets windowset
    w <- dmenuWS
    if W.tagMember w s
       then windows $ W.greedyView w
       else addWorkspace w


withWorkspace' :: (WorkspaceId -> X ()) -> X()
withWorkspace' job = do
    ts <- getTagSortedWS
    ws <- dmenuWS
    if ws `elem` ts
       then job ws
       else addHiddenWorkspace ws >> job ws


spawnSelected' :: [Cell] -> X()
spawnSelected' g = gridselect gsConf g >>= flip whenJust spawn


shellGrid :: Grid
shellGrid =
  let (names, cmds) = unzip cells
   in zip names (map (\s -> term ++ " -e " ++ s) cmds)
  where
    hledger = "hledger-ui -f ~/finance/.hledger.journal"
    cells :: [Cell]
    cells =
      [ ("Ipython", "ipython")
      , ("GHCi", "ghci")
      , ("Radian", "radian")
      , ("GiNsh", "ginsh")
      , ("Basic Calculator", "bc -q")
      , ("Maxima", "maxima")
      , ("Cmus", "cmus")
      , ("Calcurse", "calcurse")
      , ("GCal", "gcalcli")
      , ("FriCAS", "fricas")
      , ("Gap", "gap")
      , ("Sc-im", "sc-im")
      , ("Neomutt", "neomutt")
      , ("VisiData", "vd")
      , ("WeeChat", "weechat")
      , ("Gnuplot", "gnuplot")
      , ("Htop", "htop")
      , ("Btop++", "btop")
      , ("S-tui", "s-tui")
      , ("Termshark", "termshark")
      , ("Ncdu", "ncdu")
      , ("Cfdisk", "cfdisk")
      , ("Lvm Manager", "lvm")
      , ("Ranger", "ranger")
      , ("Hledger", hledger)
      ]

appGrid :: Grid
appGrid = shellGrid ++ cells
  where
    cells :: [Cell]
    cells =
      [ ("Vimiv", "vimiv")
      , ("Spotify", "spotify")
      , ("Gimp", "gimp")
      , ("wxMaxima", "wxmaxima")
      , ("Xasy", "xasy")
      , ("Inkscape", "inkscape")
      , ("Trader Workstation", "tws")
      , ("TradingView", "tradingview")
      ]

dotfilesGrid :: Grid
dotfilesGrid =
  let (names, paths) = unzip cells
   in zip names (map (\p -> term ++ " -e " ++ editor ++ " " ++ p) paths)
  where
    cells :: [Cell]
    cells =
      [ ("Neovim", "~/.config/nvim")
      , ("Kitty", "~/.config/kitty/kitty.conf")
      , ("Ranger", "~/.config/ranger/rc.conf")
      , ("Bash", "~/.bashrc")
      , ("Qutebrowser", "~/.config/qutebrowser")
      , ("XMonad", "~/.config/xmonad/xmonad.hs")
      , ("KMonad", "~/.config/kmonad")
      , ("i3", "~/.config/i3/config")
      , ("py3status", "~/.config/py3status/config")
      , ("xmobar", "~/.config/xmobar")
      , ("Zathura", "~/.config/zathura/zathurarc")
      ]

bookmarkGrid :: Grid
bookmarkGrid =
  let (names, urls) = unzip cells
   in zip names (map (\u -> browser ++ " https://" ++ u) urls)
  where
    cells :: [Cell]
    cells =
      [ ("Gmail", "mail.google.com")
      , ("Posteo", "posteo.de/en")
      , ("LessWrong", "lesswrong.com")
      , ("Leetcode", "leetcode.com")
      , ("Python Documentation", "docs.python.com")
      , ("Haskell Wiki", "wiki.haskell.org")
      , ("Vim cheatsheet", "vim.rotrr.com")
      , ("Neovim Documenation", "neovio.io/doc")
      , ("Git Documentation", "git-scm.com/doc")
      , ("Arch Wiki", "wiki.archlinux.org")
      , ("Linux manpages", "linux.die.net/man")
      , ("Wikichip", "wikichip.org")
      , ("Libgen", "libgen.is")
      , ("Sci-hub", "sc-hubtw.hkvisa.net")
      , ("Wikibooks", "wikibooks.org")
      , ("Open Library", "openlibrary.org")
      , ("arXiv", "arxiv.org")
      , ("SSRN", "ssrn.com")
      , ("JSTOR", "jstor.org")
      , ("Lexis Nexis", "lexisnexis.org")
      , ("ACM Library", "dl.acm.org")
      , ("CIA World Factbook", "cia.gov/the-world-factbook")
      , ("Project Euler", "projecteuler.net")
      , ("Wolfram Mathworld", "mathworld.wolfram.com")
      ]

gsConf :: GSConfig String
gsConf =
  def
    { gs_cellheight = 40
    , gs_cellwidth = 200
    , gs_cellpadding = 6
    , gs_originFractX = 0.5
    , gs_originFractY = 0.5
    , gs_font = xmFont
    }

gsLaunch :: Grid -> X()
gsLaunch = spawnSelected'

xmModes :: [Mode]
xmModes = [xmExitMode, xmLaunchMode, xmWorkspaceMode, xmResizeMode]

-- letter: focus workspace beginning with letter 'a' to 'z'
-- shift+letter: move to workspace beginning with letter 'a' to 'z'
xmWorkspaceMode :: Mode
xmWorkspaceMode =
    let focus = zip (zip (repeat noMod) [xK_a..xK_z])
                    (map (withLetWorkspace W.greedyView) ['a'..'z'])
        move  = zip (zip (repeat shift) [xK_a..xK_z])
                    (map (withLetWorkspace W.shift) ['a'..'z'])
        nums  = [   (( m .|. noMod, k), windows $ f i)
                |   (i, k) <- zip (map show [1..9]) [xK_1..xK_9]
                ,   (f, m) <- [(W.greedyView, 0), (W.shift, shift)]
                ]
     in mode "workspace" $ \cfg -> M.fromList $ focus ++ move ++ nums

--refactor
withLetWorkspace :: (String -> WindowSet -> WindowSet) -> Char -> X ()
withLetWorkspace job fstLet = do
  ws      <- gets (map W.tag . W.hidden . windowset)
  current <- gets (W.currentTag . windowset)
  let appJob ws =
        case take 1 $ filter (\w -> fstLet == head w) ws of
          (w:_) -> windows $ job w
          []    -> return ()
   in if head current == fstLet
        then appJob $ filter (/= current) ws
        else appJob ws

exitMsg :: String
exitMsg = "(l)ock;(p)oweroff;(r)eboot;(s)uspend;(h)ibernate;(e)xit;"

xmExitMode :: Mode
xmExitMode =  mode "exit" $ \cfg -> M.fromList lst
    where
        lst :: [KeyMap]
        lst =
          [ ((noMod, xK_l), spawn "xscreensaver-command -lock")
          , ((noMod, xK_p), spawn "systemctl poweroff")
          , ((noMod, xK_r), spawn "systemctl reboot")
          , ((noMod, xK_s), spawn "systemctl suspend")
          , ((noMod, xK_h), spawn "systemctl hibernate")
          , ((noMod, xK_x), io exitSuccess)
          ]

xmLaunchMode :: Mode
xmLaunchMode = mode "launch" $ \cfg -> M.fromList lst
  where
    lst :: [KeyMap]
    lst =
      let runExit   str = runInTerm' str >> exitMode
          spawnExit str = spawn str      >> exitMode
       in [ ((noMod, xK_f), runExit   "ranger")
          , ((noMod, xK_h), runExit   "htop")
          , ((noMod, xK_e), runExit   "nvim /tmp/tmp.txt")
          , ((noMod, xK_c), runExit   "bc -q" )
          , ((noMod, xK_s), spawnExit "spotify-launcher")
          , ((noMod, xK_b), spawnExit "qutebrowser")
          , ((noMod, xK_w), spawnExit "firefox")
          , ((modm, xK_w),  spawnExit "chromium")
          ]

xmResizeMode :: Mode
xmResizeMode = mode "resize" $ \cfg -> M.fromList lst
    where
        lst =
          [ ((noMod, xK_Left), sendMessage Expand)
          , ((noMod, xK_Right), sendMessage Shrink)
          -- , ((noMod, xK_Up),  sendMessage MirrorExpand)
          -- , ((noMod, xK_Down), sendMessage MirrorSrhink)
          ]

xmPromptMode :: Mode
xmPromptMode = mode "prompt" $ \cfg -> M.fromList lst
    where
        lst = []


rmKeys :: [KeyPair]
rmKeys =
  [ (modm, xK_Return)
  , (modShift, xK_Return)
  , (modm, xK_comma)
  , (modShift, xK_comma)
  , (modm, xK_period)
  , (modShift, xK_period)
  , (modm, xK_slash)
  , (modShift, xK_slash)
  , (modm, xK_space)
  -- , (modShift, xK_space)
  ]
  ++ zip (repeat modm)      [xK_a..xK_z]
  ++ zip (repeat modShift)  [xK_a..xK_z]
  ++ zip (repeat modm)      [xK_1..xK_9]
  ++ zip (repeat modShift)  [xK_1..xK_9]


addKeys :: [KeyMap]
addKeys =
  [ ((modm, xK_h),             sendMessage $ Go L)
  , ((modm, xK_j),             sendMessage $ Go D)
  , ((modm, xK_k),             sendMessage $ Go U)
  , ((modm, xK_l),             sendMessage $ Go R)
    -- vi-style move focus
  , ((modShift, xK_h),         sendMessage $ Swap L)
  , ((modShift, xK_j),         sendMessage $ Swap D)
  , ((modShift, xK_k),         sendMessage $ Swap U)
  , ((modShift, xK_l),         sendMessage $ Swap R)
    -- vi-style swap windows
  , ((modm, xK_t),             sendMessage ToggleStruts)
  , ((modShift, xK_t),         withFocused $ windows . W.sink)

  , ((alt, xK_Tab),            windows W.focusUp)
  , ((altShift, xK_Tab),       windows W.focusDown)
    -- alt-tab cycle focus
  , ((modShift, xK_g),         gotoMenuArgs goto_args)
  , ((modShift, xK_b),         bringMenuArgs bring_args)

  , ((modShift, xK_comma),     sendMessage Shrink)
    -- "<" == shrink
  , ((modShift, xK_period),    sendMessage Expand)
    -- ">" == grow

  , ((modm, xK_space),         sendMessage NextLayout)
  -- , ((modShift,xK_space),    setLayout $ )

    -- fine with prompt
  , ((modm, xK_a),             appendWorkspacePrompt centeredPrompt)
  , ((modm, xK_r),             renameWorkspace centeredPrompt)
    -- redo with dmenu
  -- , ((modm, xK_s),             selectWorkspace bottomPrompt)
  , ((modm, xK_s),             selectWorkspace')
  , ((modShift, xK_w),         withWorkspace' (windows . W.shift))
  , ((modCtrl, xK_w),          withWorkspace' (windows . copy))
  -- , ((modShift, xK_w),         withWorkspace bottomPrompt (windows . W.shift))
  -- , ((modCtrl, xK_w),          withWorkspace bottomPrompt (windows . copy))
  , ((modShift, xK_BackSpace), removeWorkspace)
    -- dynamic workspaces commands

  , ((modCtrl, xK_plus),       sendMessage MagnifyMore )
  , ((modCtrl, xK_minus),      sendMessage MagnifyLess)
  , ((modCtrl, xK_m),          sendMessage Toggle)
    -- magnify commands

  , ((modm, xK_underscore),    withFocused minimizeWindow)
    -- minimize commands

  , ((modm, xK_c),             gsLaunch dotfilesGrid)
  , ((modm, xK_m),             gsLaunch bookmarkGrid)
  , ((modShift, xK_o),         gsLaunch appGrid)
    -- grid select commands debug
  , ((modShift, xK_c),         spawn reload_xmonad)
  , ((modm, xK_q),             kill)

  , ((modm, xK_w),             setMode "workspace")
  , ((modm, xK_o),             setMode "launch")
  , ((modShift, xK_x),         setMode "exit")
  , ((modShift, xK_r),         setMode "resize")
    -- set modes

  , ((modm,     xK_Return),    spawn term)
  , ((modShift, xK_Return),    runInBash "'cf ~'")
  , ((modAlt,   xK_Return),    runInBash "'cf /'")
  , ((modCtrl,  xK_Return),    runInBash "'rcd'")

  , ((modm, xK_d),             spawn dmenu_run)

    -- TODO Debug these or look into image magick import
  , ((noMod, xK_Print),        unGrab *> spawn scrot_screen)
  , ((modm, xK_Print),         unGrab *> spawn scrot_focused)
  , ((modShift, xK_Print),     unGrab *> spawn scrot_select)

  , ((noMod, xK_Insert),       pasteSelection)
  ]
  -- ++ -- switch to ws at index n debug
  -- zip (zip (repeat modm) [xK_1..xK_9])
  --     (map (withWorkspaceIndex W.greedyView) [1..])
  -- ++ -- set index N to the current workspace debug
  -- zip (zip (repeat (modm .|. ctrl)) [xK_1..xK_9])
  --     (map setWorkspaceIndex [1..])
  ++ -- switch ws between screens
  [ ((m .|. modm, k), screenWorkspace s >>= flip whenJust (windows . f))
  | (k, s) <- zip [xK_bracketleft, xK_bracketright] [0..]
  , (f, m) <- [(W.view, 0), (W.shift, shift)]
  ]
    where
        dmenuDefaultArgs, goto_args :: [String]
        dmenuDefaultArgs = ["-b", "-h", "20"]
        goto_args     = dmenuDefaultArgs ++ ["-p", "Where to?"]
        bring_args    = dmenuDefaultArgs ++ ["-p", "Which shall I bring?"]
        dmenu_run     = "dmenu_run -b -h 20 -p 'Yes, Master?'"
        reload_xmonad = "xmonad --recompile; xmonad --restart"
        scrot_screen  = "sleep 0.2; scrot -q 100 --file "     ++ scrot_file
        scrot_select  = "sleep 0.2; scrot -sf -q 100 --file " ++ scrot_file
        scrot_focused = "sleep 0.2; scrot -u -q 100 --file "  ++ scrot_file
        scrot_file    = "~/pictures/screnshots/%Y-%m-%d-%T-screenshot.png"


-- use loginctl? proper order to start these programs?
-- seems to work fine
-- start some nice programs
xmStartupHook :: X ()
xmStartupHook = spawnInitProgs


spawnInitProgs :: X()
spawnInitProgs = do
  spawnOnce "picom"
  -- systray apps?
  -- spawnOnce "pasystray"
  -- spawnOnce "blueman-applet"
  -- spawnOnce "nm-applet --sm-disable --indicator"

addInitWS :: X()
addInitWS = do
    appendWorkspace "spotify"
    spawnOn         "spotify" "spotify-launcher"
    appendWorkspace "email"
    -- spawnOn         "emai"

-- Note that each layout is separated by ||| which denotes layout choice.
xmLayoutHook =
  mouseResize
    . windowArrange
    . windowNavigation
    . avoidStruts
    . magnifier
    . minimize
    $ smartBorders layouts
    where
        layouts = noBorders Full ||| BSP.emptyBSP ||| spiral (6/7)

-- not quite done tweaking layout list yet, also renaming layouts?
-- nested layouts?
-- Toggle layouts?

-- create some X window rules:
-- look into the hook helpers import
xmManageHook :: Query (Endo WindowSet)
xmManageHook = composeAll
  [ isDialog                                     --> doFloat
  , className =? "mpv"                           --> doFloat
  , className =? "Gimp"                          --> doFloat
  , className =? "toolbar"                       --> doFloat
  , className =? "confirm"                       --> doFloat
  , className =? "error"                         --> doFloat
  , className =? "download"                      --> doFloat
  , className =? "notification"                  --> doFloat
  , className =? "Toolkit"                       --> doFloat
  , className =? "Xmessage"                      --> doFloat
  , className =? "pinentry"                      --> doFloat
  , className =? "pinentry-qt"                   --> doFloat
  , title     =? "Oracle VM VirtualBox Manager"  --> doFloat
  ]

xmEventHook :: Event -> X All
xmEventHook = swallowEventHook query (return True)
  where
      query = className =? "kitty" <||> className =? "alacritty"

-- logging: perform an arbitrary action on each internal state change
-- or X event
xmLogHook :: X ()
xmLogHook = workspaceHistoryHook

centeredPrompt :: P.XPConfig
centeredPrompt =
  def
    { P.font              = xmFont
    , P.bgColor           = "#000000"
    -- , P.fgColor           = "#ffffff"
    , P.fgColor           = xmFBC
    , P.borderColor       = xmFBC
    , P.promptBorderWidth = 2
    , P.position          = P.CenteredAt (1/2) (1/2)
    , P.height            = 20
    , P.autoComplete      = Just 1
    }

bottomPrompt :: P.XPConfig
bottomPrompt =
  def
    { P.font              = xmFont
    , P.bgColor           = "#000000"
    , P.fgColor           = "#ffffff"
    , P.borderColor       = xmFBC
    , P.promptBorderWidth = 1
    , P.position          = P.Bottom
    , P.height            = 20
    , P.autoComplete      = Just 1
    }

-- xmPPLayout :: T.Text -> T.Text
-- xmPPLayout layout
--   | "Full"   `T.isInfixOf` layout = "Full"
--   | "BSP"    `T.isInfixOf` layout = "BSP"
--   | "Spiral" `T.isInfixOf` layout = "Spiral"
--   | "Mosaic" `T.isInfixOf` layout = "Mosaic"
--   | otherwise = const ""
--
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

xmobarAlternate = let cmd = home ++ "/.local/bin/xmobar-1"
                   in statusBarPropTo "_XMONAD_LOG_1" cmd (pure xmPP)

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
        , handleEventHook = xmEventHook
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

