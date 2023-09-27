Config {

   -- appearance
     font =         "xft:Mononoki Nerd Font-11:bold:antialias=true:hinting=true"
   , additionalFonts = [
                         "xft:JoyPixels:size=9:antialias=true:hinting=true"
                         ,"xft:FontAwesome:size=9:antialias=true:hinting=true"
                       ]
   , bgColor =      "#282828"
   , fgColor =      "#ebdbb2"
   , position =     Top
   , border =       BottomB
   , borderColor =  "#282828"
   , iconRoot = "."
   , sepChar =  "%"   -- delineator between plugin names and straight text
   , alignSep = "]["  -- separator between left-right alignment
   , template = " %StdinReader% ][ %/home/jk/.config/xmobar/bin/xmobarstatus%"
   , lowerOnStart =     False    -- send to bottom of window stack on start
   , hideOnStart =      False   -- start with window unmapped (hidden)
   , allDesktops =      True    -- show on all desktops
   , overrideRedirect = True    -- set the Override Redirect flag (Xlib)
   , pickBroadest =     False   -- choose widest display (multi-monitor)
   , persistent =       False    -- enable/disable hiding (True = disabled)
   , commands =
        , Run Com "xmobarstatus" [] "" 10
        , Run Memory         [ "--template" ,"Mem: <usedratio>%"
                             , "--Low"      , "20"        -- units: %
                             , "--High"     , "90"        -- units: %
                             , "--low"      , "#8ec07c"
                             , "--normal"   , "#fabd2f"
                             , "--high"     , "#fb4934"
                             ] 10
        , Run Battery        [ "--template" , "Batt: <acstatus>"
                             , "--Low"      , "10"        -- units: %
                             , "--High"     , "80"        -- units: %
                             , "--low"      , "darkred"
                             , "--normal"   , "#fabd2f"
                             , "--high"     , "#8ec07c"

                             , "--" -- battery specific options
                                       -- discharging status
                                       , "-o"	, "<left>% (<timeleft>)"
                                       -- AC "on" status
                                       , "-O"	, "<fc=#fabd2f>Charging</fc>"
                                       -- charged status
                                       , "-i"	, "<fc=#8ec07c>Charged</fc>"
                             ] 50

        , Run Date           "<fc=#b16286>%F (%a) %T</fc>" "date" 10
        , Run StdinReader
        , Run Brightness
              [ "-t", "<ipat>"
              , "--"
              , "--brightness-icon-pattern", "<icon=bright_%/home/horhik/Pictures/test.xpm%/>"
              ] 30
        ]
   }
