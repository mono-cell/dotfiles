Config { font = "xft:UbuntuCondensed Nerd Font:size=12"
   , additionalFonts = [
                         "xft:JoyPixels:size=12"
                       ]
   , bgColor =      "#0c0c0c"
   , fgColor =      "#ebdbb2"
   , position = Static { xpos = 3 , ypos = 3, width = 1360, height = 30 }
   , iconRoot         = "~/.config/xmonad/icons"
   , border =       BottomB
   , borderColor =  "#4a4a4a"
   , sepChar =  "%"   -- delineator between plugin names and straight text
   , alignSep = "]["  -- separator between left-right alignment
   , template = "  %UnsafeStdinReader% ][ %date% "
   , lowerOnStart =     True    -- send to bottom of window stack on start
   , hideOnStart =      False   -- start with window unmapped (hidden)
   , allDesktops =      True    -- show on all desktops
   , overrideRedirect = True    -- set the Override Redirect flag (Xlib)
   , iconOffset       = 1
   , pickBroadest =     False   -- choose widest display (multi-monitor)
   , textOffsets      = [20, 22, 22, 21, 22]
   , persistent =       True    -- enable/disable hiding (True = disabled)
   , commands =
        [ Run Date "%l:%M %p " "date" 10
        , Run UnsafeStdinReader
        ]
   }
