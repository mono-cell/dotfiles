Config {
     -- font =         "xft:Iosevka Nerd Font :size=9:antialias=true:hinting=true"
     font = "xft:Roboto:size=8:antialias=true:hinting=True"
   , additionalFonts = [
                        "xft:UbuntuCondensed Nerd Font:size=10:antialias=true:hinting=True"
                       , "xft:Mononoki-10:bold:antialias=true:hinting=True"
                       ]
   , bgColor =      "#282828"
   , fgColor =      "#ebdbb2"
   , alpha = 255
   , position = Static { xpos = 3 , ypos = 3, width = 1366, height = 20 }
   , border =       BottomB
   , borderColor =  "#282828"
   , iconRoot = "."

   -- layout
   , sepChar =  "%"   -- delineator between plugin names and straight text
   , alignSep = "]["  -- separator between left-right alignment
   , template = " %UnsafeStdinReader%][%/home/jk/.config/xmobar/bin/xmobarstatus%"

   -- general behavior
   , lowerOnStart =     False    -- send to bottom of window stack on start
   , hideOnStart =      False   -- start with window unmapped (hidden)
   , allDesktops =      False    -- show on all desktops
   , overrideRedirect = True    -- set the Override Redirect flag (Xlib)
   , iconOffset       = 1
   , pickBroadest =     False   -- choose widest display (multi-monitor)
   , persistent =       False    -- enable/disable hiding (True = disabled)
   , commands =
        [ Run UnsafeStdinReader
        , Run Com "xmobarstatus" [] "xmobarstatus2" 10
        ]
   }
