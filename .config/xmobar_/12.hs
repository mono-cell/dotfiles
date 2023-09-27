 Config {

   -- appearance
     font =         "xft:Mononoki Nerd Font-11:bold:antialias=true:hinting=true"
   , additionalFonts = [
                         "xft:Fira Code-10:bold:antialias=true:hinting=True"
                       , "xft:Mononoki-10:bold:antialias=true:hinting=True"
                       ]
   , bgColor =      "#282828"
   , fgColor =      "#ebdbb2"
   , position =     Top
   , border =       BottomB
   , borderColor =  "#282828"
   , iconRoot = "."
   , sepChar =  "%"   -- delineator between plugin names and straight text
   , alignSep = "]["  -- separator between left-right alignment
   , template = " %StdinReader% ][%/home/jk/.config/xmobar/bin/xmobarstatus2%"
   , lowerOnStart =     False    -- send to bottom of window stack on start
   , hideOnStart =      False   -- start with window unmapped (hidden)
   , allDesktops =      True    -- show on all desktops
   , overrideRedirect = True    -- set the Override Redirect flag (Xlib)
   , pickBroadest =     False   -- choose widest display (multi-monitor)
   , persistent =       False    -- enable/disable hiding (True = disabled)
   , commands =
        [ Run Date           "<fc=#b16286>%F (%a) %T</fc>" "date" 10
        , Run StdinReader
        , Run Com "xmobarstatus2" [] "xmobarstatus2" 10
        ]
   }
