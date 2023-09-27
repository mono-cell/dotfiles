module Main (main) where

import Xmobar

config :: Config
config =
  defaultConfig
    { overrideRedirect = False
    , position = OnScreen 1 (TopW L 100)
    , border = NoBorder
    , bgColor = "#000000"
    , fgColor = "#646464"
    , borderColor = "#F8F8F2"
    , alpha = 255
    , iconRoot = "/home/carterlevo/.config/xmonad/xpm"
    , font = "xft:Anonymous Pro:weight=bold:pixelsize=14:antialias=true"
    , additionalFonts =
        [ "xft:Symbols Nerd Font:pixelsize=14:antialias=true:hinting=true" ]
    , commands =
        [ Run $ XPropertyLog "_XMONAD_LOG_1"
        , Run $ Com "sh" ["-c", "checkupdates | wc -l"] "updates" 3600
        , Run $ Date "<fn=1>\xF273</fn> %a %m-%d-%Y" "date" 10
        , Run $ Date "<fc=#FFFFFF>%H:%M</fc>" "time" 10
        , Run $ Uptime ["-t", "Up: <days>d <hours>h"] 360

        , Run $ Com "uname" ["-r"] "kernel" 36000
        , Run $ Com "userhost" [] "host" 36000
        , Run $ Com "echo" ["<fn=1>\xF0F3</fn>"] "bell" 36000
        , Run $ Com "echo" ["<fn=1>\xF303</fn>"] "arch" 36000
        , Run $ Com "echo" ["<fn=1>\xE22C</fn>"] "pi" 36000
        , Run $ Com "echo" ["<fn=1>\xFB26</fn>"] "lambda" 36000
        , Run $ Com "echo" ["<fn=1>\xF64F</fn>"] "clock" 36000
        ]
        , template =
            "<icon=arch-linux-2.xpm/> %_XMONAD_LOG_1% }{\
                \| %bell% %updates% | %time% "
        }


main :: IO ()
main = xmobar config



