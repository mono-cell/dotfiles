{-# LANGUAGE PostfixOperators #-}

import           Xmobar

config :: Config
config =
  defaultConfig
    { font = "xft:Iosevka Nerd Font-10"
    , additionalFonts =
        [ "xft:FontAwesome-10"
        , "xft:JetBrainsMono Nerd Font-14"
        ]
    , bgColor = "#171A1F"
    , fgColor = "#fefefe"
    , borderColor = "#3a3a3a"
    , border = FullB
    , position = Static{xpos = 5, ypos = 5, width = 1910, height = 35}
    , alpha = 255
    , lowerOnStart = True
    , allDesktops = True -- show on all desktops
    , overrideRedirect = True -- set the Override Redirect flag (Xlib)
    , pickBroadest = False -- choose widest display (multi-monitor)
    , hideOnStart = False
    , persistent = True
    , commands = myCommands
    , sepChar = "%"
    , alignSep = "}{"
    , template         =  " %UnsafeXMonadLog% }{<fc=#95b1a3>%upd%</fc>  <fc=#95b1a3>%bt%</fc>  <fc=#bb6c73>•</fc>  <fc=#95b1a3>%memory%</fc>  <fc=#bb6c73>•</fc>  %dte% <fc=#bb6c73>•</fc> "
     }


myCommands :: [Runnable]
myCommands =
  [ Run UnsafeXMonadLog
  , Run $ Com "/home/jk/.config/xmobar/scripts/updates.sh" [] "upd" 100
  , Run $ Com "/home/jk/.config/xmobar/scripts/dte.sh" [] "dte" 100
  , Run $ Com "/home/jk/.config/xmobar/scripts/battery.sh" [] "bt" 100
  ]

main :: IO ()
main = xmobar config
