module Main (main) where

import Xmobar

import Control.Monad

import System.Console.GetOpt
import System.IO
import System.Exit
import System.Environment

type ScreenId = Int

data Conf = Main | Alt

data Flags = Flags { optConf   :: Conf
                   , optScreen :: Int
                   , optHelp   :: Bool
                   }

mainFlags, altFlags :: Flags
mainFlags  = Flags { optConf = Main
                   , optScreen = 0
                   , optHelp = False
                   }

altFlags   = Flags { optConf = Alt
                   , optScreen = 1
                   ,  optHelp = False
                   }


mainTemplate, altTemplate :: ScreenId -> String
mainTemplate sc = let log = "%_XMONAD_LOG_" ++ show sc ++ "%"
                   in "<icon=arch-linux-2.xpm/> " ++ log ++ " }{\
                        \| %arch% CPU: %cpu% \
                        \| %memory% | %clock% %uptime% \
                        \| %date% | %time% | %lambda% "

altTemplate sc = let log = "%_XMONAD_LOG_" ++ show sc ++ "%"
                  in "<icon=arch-linux-2.xpm/> " ++ log ++ " }{\
                       \| %bell% %updates% | %time% | %lambda% "

help :: [String]
help = []

temp :: String
temp =
  "<icon=arch-linux-2.xpm/> %_XMONAD_LOG_0% }{\
    \| %arch% CPU: %cpu% | %memory% \
    \| %clock% %uptime% | %date% | %time% | %lambda% "

baseCommands :: [Runnable]
baseCommands =
  [ Run $ Cpu
      [ "-t", "<total>%"
      , "-H", "75"
      , "-L", "25"
      , "-h", "red"
      , "-l", "green"
      , "-n", "white"
      ]
      10
  , Run $ CpuFreq
      [ "-t", "<avg>GHz"
      , "-H", "4"
      , "-L", "1"
      , "-h", "red"
      , "-l", "green"
      , "-n", "white"
      ]
      50
  , Run $ Memory
      ["-t", "<fn=1>\xF233</fn> Mem: <used>M (<usedratio>%)"]
      10
  , Run $ Swap
      ["-t", "<fn=1>\xF9E1</fn> Swap: <usedratio>%"]
      10
  , Run $ Uptime
      ["-t", "Up: <days>d <hours>h"]
      360
  , Run $ Date
      "<fn=1>\xF273</fn> %a %m-%d-%Y"
      "date"
      10
  , Run $ Date
      "<fc=#FFFFFF>%H:%M</fc>"
      "time"
      10
  ]

shellCommands :: [Runnable]
shellCommands =
  [ Run $ Com "echo"     ["<fn=1>\xF303</fn>"]          "arch"    36000
  , Run $ Com "echo"     ["<fn=1>\xE22C</fn>"]          "pi"      36000
  , Run $ Com "echo"     ["<fn=1>\xFB26</fn>"]          "lambda"  36000
  , Run $ Com "echo"     ["<fn=1>\xF64F</fn>"]          "clock"   36000

  , Run $ Com "uname"    ["-r"]                         "kernel"  36000
  , Run $ Com "userhost" []                             "host"    36000

  , Run $ Com "sh"       ["-c", "checkupdates | wc -l"] "updates" 3600
  ]


logCommand :: ScreenId -> [Runnable]
logCommand sc = let log = "_XMONAD_LOG_" ++ show sc
                 in [ Run $ XPropertyLog log ]

config :: Config
config =
    defaultConfig
      { overrideRedirect = False
      , border           = NoBorder
      , bgColor          = "#000000"
      , fgColor          = "#646464"
      , borderColor      = "#F8F8F2"
      , alpha            = 255
      , iconRoot         = "/home/jk/.config/xmonad/xpm"
      , font             = "xft:Iosevka Nerd Font-12"
      , additionalFonts  =
          [ "xft:Symbols Nerd Font:pixelsize=14:antialaias=true:hinting=true" ]
      -- , position         = OnScreen 0 (TopW L 100)
      -- , commands = baseCommands
      -- , template = defaultTemplate
      }


main :: IO ()
main = xmobar config { position = OnScreen 0 (TopW L 100)
                     , commands = baseCommands ++ logCommand 0
                     , template = mainTemplate 0
                     }
