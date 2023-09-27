module Main (main) where

import Xmobar

config :: Config
config =
    defaultConfig
      { overrideRedirect = False
      , position = OnScreen 0 (TopW L 100)
      , border = NoBorder
      , bgColor = "#000000"
      , fgColor = "#646464"
      , borderColor = "#F8F8F2"
      , alpha = 255
      , iconRoot = "/home/carterlevo/.config/xmonad/xpm"
      , font = "xft:Anonymous Pro:weight=bold:pixelsize=14:antialias=true"
      , additionalFonts =
          [ "xft:Symbols Nerd Font:pixelsize=14:antialaias=true:hinting=true" ]
      , commands =
          [ Run $ XPropertyLog "_XMONAD_LOG_0"
          , Run $ Cpu
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

          , Run $ Date "<fn=1>\xF273</fn> %a %m-%d-%Y" "date" 10
          , Run $ Date "<fc=#FFFFFF>%H:%M</fc>" "time" 10
          , Run $ Uptime ["-t", "Up: <days>d <hours>h"] 360

          , Run $ Com "sh" ["-c", "checkupdates | wc -l"] "updates" 3600

          , Run $ Com "uname" ["-r"] "kernel" 36000
          , Run $ Com "userhost" [] "host" 36000
          , Run $ Com "echo" ["<fn=1>\xF303</fn>"] "arch" 36000
          , Run $ Com "echo" ["<fn=1>\xE22C</fn>"] "pi" 36000
          , Run $ Com "echo" ["<fn=1>\xFB26</fn>"] "lambda" 36000
          , Run $ Com "echo" ["<fn=1>\xF64F</fn>"] "clock" 36000
          ]
      , template =
          "<icon=arch-linux-2.xpm/> %_XMONAD_LOG_0% }{\
            \| %arch% CPU: %cpu% | %memory% \
            \| %clock% %uptime% | %date% | %time% "
      }


main :: IO ()
main = xmobar config
