{-# LANGUAGE ScopedTypeVariables #-}

import Xmobar

config :: Config
config = defaultConfig
      { font = "xft:Iosevka Nerd Font-10"
      , additionalFonts =
          [ "xft:FontAwesome-10"
          , "xft:JetBrainsMono Nerd Font-14"
          ]
      , bgColor = "#212121"
      , fgColor = "white"
      , borderColor = "#3a3a3a"
      , border = FullB
      , position = Static{xpos = 5, ypos = 5, width = 1430, height = 35}
      , borderWidth = 0
      , sepChar = "%" -- delineator between plugin names and straight text
      , alignSep = "}{" -- separator between left-right alignment
      , template =
          " \62227 \57533 \59255 \57533 %XMonadLog% \57533 }{ %battery%"
            <> "\57533 %date% \57533 %memory% "
      , lowerOnStart = True -- send to bottom of window stack on start
      , hideOnStart = False -- start with window unmapped (hidden)
      , allDesktops = True -- show on all desktops
      , overrideRedirect = True -- set the Override Redirect flag (Xlib)
      , pickBroadest = False -- choose widest display (multi-monitor)
      , persistent = True -- enable/disable hiding (True = disabled)
      , commands =
          [ -- XMonad logs
            Run XMonadLog
          , Run $
              Memory
                [ "--template"
                , "\983899 <usedratio> %" -- 󰍛
                , "--Low"
                , "20" -- units: %
                , "--High"
                , "90" -- units: %
                , "--low"
                , okColour
                , "--normal"
                , warnColour
                , "--high"
                , criticalColour
                ]
                10
          , Run $
              Battery
                [ "--template"
                , "<acstatus>"
                , "--Low"
                , "10" -- units: %
                , "--High"
                , "80" -- units: %
                , "--low"
                , criticalColour
                , "--normal"
                , warnColour
                , "--high"
                , okColour
                , "--" -- battery specific options
                -- discharging status 󰁿
                , "-o"
                , "\983167 <left>% (<timeleft>)"
                , -- AC "on" status, charging 󰂄
                  "-O"
                , "<fc=" <> warnColour <> ">\983172</fc>"
                , -- charged status 󰁹
                  "-i"
                , "<fc=" <> okColour <> ">\983161</fc>"
                ]
                50
          , Run $ Date "<fc=#FFFFFF>%F (%a) %T</fc>" "date" 10
          ]
      }
  where
    criticalColour = "#FF5370"
    warnColour = "#E6B455"
    okColour = "#B480D6"

main :: IO ()
main = xmobar config
