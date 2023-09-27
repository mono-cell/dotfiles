Config { font = "xft:JetBrainsMono Nerd Font:size=9"
       , additionalFonts =
          [ "xft:JetBrainsMono Nerd Font:size=9"
          , "xft:FontAwesome:pixelsize=9:bold"
          , "xft:FontAwesome:pixelsize=9"
          , "xft:Hack Nerd Font Mono:pixelsize=9"
          , "xft:Hack Nerd Font Mono:pixelsize=9"
          ]
       , border = NoBorder
       , bgColor = "#2B2E37"
       , fgColor = "#929AAD"
       , alpha = 255
       , position = TopSize L 100 30
       --, textOffset = 1
       --, textOffsets = [ 1, 1 ]
       , lowerOnStart = True
       , allDesktops = True
       , persistent = False
       , hideOnStart = False
       , iconRoot = "/home/jk/.config/xmonad/xmobar/icons/"
       , commands =
         [ Run UnsafeXPropertyLog "_XMONAD_LOG_0"
         , Run Date "%a, %d %b   <fn=1>-</fn>   %l:%M %p" "date" 10
         , Run Memory ["-t","Mem: <fc=#AAC0F0><usedratio></fc>%"] 10
	 , Run Battery [
	"-t", "<acstatus>: <left>%",
	"--",
	--"-c", "charge_full",
	"-O", "AC",
	"-o", "Bat",
	"-h", "green",
	"-l", "red"
	] 10
         ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " %_XMONAD_LOG_0% } %date% { %memory% %battery%"
       }
