from libqtile import bar, layout, widget, hook
from libqtile.config import Click, Drag, Group, Key, KeyChord, Match, Screen, ScratchPad, DropDown
from libqtile.lazy import lazy

import os, subprocess, psutil

mod = "mod4"

myTerminal ="alacritty"
myShell = "fish"
myBrowser = "firefox" # Firefox with cascade custom userChrome.css <3
myFM = "pcmanfm"

group_names = '    󰎆'.split()

catppuccin = {
    "flamingo": "#f2cdcd",
    "mauve": "#cba6f7",
    "rosewater": "#f5e0dc",
    "pink": "#f5c2e7",
    "maroon": "#eba0ac",
    "red": "#f38ba8",
    "peach": "#fab387",
    "yellow": "#f9e2af",
    "green": "#a6e3a1",
    "teal": "#94e2d5",
    "blue": "#89b4fa",
    "sky": "#89dceb",
    "white": "#cdd6f4",
    "gray": "#6e6c7e",
    "black": "#1e1e2e",
    "transparent": "#00000000",
}

@lazy.function
def increase_gaps(qtile):
    qtile.current_layout.margin += 10
    qtile.current_group.layout_all()

@lazy.function
def decrease_gaps(qtile):
    qtile.current_layout.margin -= 10
    qtile.current_group.layout_all()

keys = [

    # Switch between windows
    Key([mod], "h",
        lazy.layout.decrease_ratio(),
        desc="Decrease Ratio"),
    Key([mod], "l",
        lazy.layout.increase_ratio(),
        desc="Increase Ratio"),
    Key([mod], "j",
        lazy.layout.down(),
        desc="Move focus down"),
    Key([mod], "k",
        lazy.layout.up(),
        desc="Move focus up"),

    Key([mod], "d",
        lazy.layout.decrease_nmaster(),
        desc="Decrease Number of Master"),
    Key([mod], "i",
        lazy.layout.increase_nmaster(),
        desc="Increase Number of Master"),
    Key([mod, "shift"], "space", lazy.window.toggle_floating()),

    Key(
        [mod], "m",
        lazy.group.setlayout('max'),
        desc="Switch to Max Layout"),

    Key(
        [mod], "t",
        lazy.group.setlayout('tile'),
        desc="Switch to Tile Layout"),
    Key(
        [mod], "f",
        lazy.group.setlayout('floating'),
        desc="Switch to Floating Layout"),
    # Move windows between left/right columns or move up/down in current stack.
    # Moving out of range in Columns layout will create new column.
    Key([mod, "shift"], "j",
        lazy.layout.shuffle_down(),
        desc="Move window down"),

    Key([mod, "shift"], "k",
        lazy.layout.shuffle_up(),
        desc="Move window up"),
    Key([mod, "shift"],"f",
        lazy.window.toggle_fullscreen(),
        desc="Toggle Fullscreen"),

    Key([mod,"control"], "j",
        increase_gaps(),
        desc="Increase gaps"),

    Key([mod,"control"], "k",
        decrease_gaps(),
        desc="Decrease gaps"),

   #Application launching Keybinds
    Key([mod], "Return",
        lazy.spawn(myTerminal),
        desc="Launch Terminal"),
    Key([mod], "b",
        lazy.spawn(myBrowser),
        desc="Launch Browser"),
    Key([mod], "e",
        lazy.spawn(myFM),
        desc="Launch File Manager"),
    Key([mod,"shift"], "d",
        lazy.spawn("rofi -show drun"),
        desc="Launch Rofi"),
    Key([mod,"shift"], "w",
        lazy.spawn("setbg"),
        desc="Random Wallpaper"),
    Key([mod], "p",
        lazy.spawn("dmenu_run"),
        desc="Launch Dmenu"),
    Key([mod], "space",
        lazy.spawn("emacsclient -c -a emacs"),
        desc="Launches EmacsClient"),

    # Toggle between different layouts as defined below
    Key([mod], "q",
        lazy.window.kill(),
        desc="Kill focused window"),
    Key([mod, "shift"], "r",
        lazy.reload_config(),
        desc="Reload the config"),
    Key([mod, "shift"], "q",
        lazy.shutdown(), desc="Shutdown Qtile"),

]


#This Function is responsible for changing names of the groups
#By Default it assigns monadt all layout to all of the groups/workspaces
groups = [Group(name, layout='tile') for name in group_names]
for i, name in enumerate(group_names):
    indx = str(i + 1)
    keys += [
        Key([mod], indx, lazy.group[name].toscreen()),
        Key([mod, 'shift'], indx, lazy.window.togroup(name))
    ]

groups.append(ScratchPad("scratchpad",[
    DropDown("terminal", f"{myTerminal} --class=s-terminal", width=0.8, height=0.8, x=0.1, y=0.1, opacity=1),
    DropDown("btop", f"{myTerminal} --class=s-btop -e btop", width=0.8, height=0.8, x=0.1, y=0.1, opacity=1),
    DropDown("mpd", f"{myTerminal} --class=s-mpd -e ncmpcpp", width=0.8, height=0.8, x=0.1, y=0.1, opacity=1),
    DropDown("ranger", f"{myTerminal} --class=s-ranger -e ranger", width=0.8, height=0.8, x=0.1, y=0.1, opacity=1),
    DropDown("newsboat", f"{myTerminal} --class=s-newsboat -e newsboat", width=0.8, height=0.8, x=0.1, y=0.1, opacity=1),
]))

keys.extend([
    KeyChord([mod], "s", [
        Key([], "t", lazy.group['scratchpad'].dropdown_toggle('terminal')),
        Key([], "h", lazy.group['scratchpad'].dropdown_toggle('btop')),
        Key([], "m", lazy.group['scratchpad'].dropdown_toggle('mpd')),
        Key([], "n", lazy.group['scratchpad'].dropdown_toggle('newsboat')),
        Key([], "e", lazy.group['scratchpad'].dropdown_toggle('ranger')),
    ])])

myBarWidgets=[
    widget.GroupBox(
        borderwidth = 2,
        disable_drag = True,
        use_mouse_wheel = False,
        rounded = False,
        active = catppuccin["peach"],
        inactive = catppuccin["gray"],
        block_highlight_text_color = catppuccin["green"],
        highlight_color = [catppuccin["black"], catppuccin["black"]],
        this_current_screen_border = catppuccin["green"],
        highlight_method = "line",
        urgent_alert_method = "line",
    ),
    widget.Sep(linewidth = 1, padding = 10, foreground = catppuccin["black"],background = catppuccin["black"]),
    widget.CurrentLayoutIcon(scale = 0.5, foreground = catppuccin["black"], background = catppuccin["red"]),
    widget.Sep(linewidth = 1, padding = 10, foreground = catppuccin["black"],background = catppuccin["black"]),
    widget.WindowName(
        font = "JetBrainsMono Nerd Font",
        foreground=catppuccin["rosewater"]
    ),
    widget.TextBox(text = "CPU:", fontsize = 12, font = "JetBrainsMono Nerd Font", foreground = catppuccin["black"], background = catppuccin["pink"]),
    widget.CPU(
        font = "JetBrainsMono Nerd Font",
        update_interval = 1.0,
        format = '{load_percent}%',
        foreground = catppuccin["pink"],
        padding = 5
    ),
    widget.Memory(
        font = "JetBrainsMono Nerd Font",
        foreground = catppuccin["green"],
        format = ' {MemUsed: .0f}{mm}',
        measure_mem='M',
        padding = 6,
    ),
    widget.Clock(format='[ %a:%d %b ]', font = "JetBrainsMono Nerd Font", padding = 6, foreground = catppuccin["yellow"]),
    widget.Clock(format='[ %l:%M %p ]', font = "JetBrainsMono Nerd Font", padding = 6, foreground = catppuccin["sky"]),
    widget.Systray(background = catppuccin["black"], icon_size = 20, padding = 4),
    widget.Sep(linewidth = 1, padding = 10, foreground = catppuccin["black"],background = catppuccin["black"]),]

screens = [
    Screen(top=bar.Bar(myBarWidgets,size=24, background=catppuccin["black"]))
]

myLayoutTheme={
    "border_focus":catppuccin["rosewater"],
    "border_normal":catppuccin["black"],
    "border_width":2,
    "margin":9,
}
layouts = [
    layout.Tile(**myLayoutTheme,ratio=0.55),
    layout.Max(),
    layout.Floating(),
]
floating_layout = layout.Floating(
    **myLayoutTheme,
    float_rules=[
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
    ]
)

mouse = [
    Drag([mod], "Button1",
         lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([mod,"shift"],
         "Button1",
         lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click([mod], "Button1",
          lazy.window.bring_to_front()),
]

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True
auto_fullscreen = False
focus_on_window_activation = "smart"
reconfigure_screens = True

# When using the Wayland backend, this can be used to configure input devices.
wl_input_rules = None


dgroups_key_binder = None
dgroups_app_rules = []
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
wmname = "LG3D"

@hook.subscribe.client_new
def _swallow(window):
    pid = window.window.get_net_wm_pid()
    ppid = psutil.Process(pid).ppid()
    cpids = {c.window.get_net_wm_pid(): wid for wid, c in window.qtile.windows_map.items()}
    for i in range(5):
        if not ppid:
            return
        if ppid in cpids:
            parent = window.qtile.windows_map.get(cpids[ppid])
            parent.minimized = True
            window.parent = parent
            return
        ppid = psutil.Process(ppid).ppid()

@hook.subscribe.client_killed
def _unswallow(window):
    if hasattr(window, 'parent'):
        window.parent.minimized = False

#This script will execute every single time either you reload qtile config or login
#Don"t add your apps like discord here. Thank me later :)
start_always ='~/.config/qtile/autostart.sh'
@hook.subscribe.startup
def autostart():
    home = os.path.expanduser(start_always)
    subprocess.Popen(["bash",home])
