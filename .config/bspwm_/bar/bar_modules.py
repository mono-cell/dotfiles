import os
import time
import json
import random
import socket
import calendar
import datetime
import requests
import threading
import subprocess
from io import StringIO
from bs4 import BeautifulSoup

from colors import modus_vivendi as cdict

from lemonbar_script import GDKSCALE


def cmd_output(cmd, **kwargs):
    try:
        out = subprocess.check_output(cmd, text=True, shell=True).strip()
    except Exception:
        out = ''
    return out


def ficon(icon, color=None, beforepad=0, afterpad=5, idx=2, swap=False):
    '''
    idx: index of font to be used
    '''
    beforepad *= GDKSCALE
    afterpad *= GDKSCALE
    return ('%{O'+f'{beforepad}'+'}'+('%{F'+color+'}' if color else '')+
            ('%{R}' if swap else '')+'%{T'+
            f'{idx}'+'}' + icon+'%{T-}'+('%{R}' if swap else '')+('%{F-}' if color else '')+'%{O'+
            f'{afterpad}'+'}')


def read_fwf(text_table):
    """
    only works when the first row is columns with a single word
    """
    cols_row = text_table.splitlines()[0]
    columns = cols_row.split()
    cols_idxs = []
    for i, (col, colnxt) in enumerate(zip(columns, columns[1:])):
        cols_idxs.append((0 if i == 0 else cols_row.index(f' {col}')+1,
                     cols_row.index(f' {colnxt}')+1))
    # cols_widths = [(cols_row.index(c), cols_row.index(cnxt)) for c, cnxt in zip(columns, columns[1:])]
    cols_idxs.append((cols_row.index(f' {columns[-1]}')+1, -1))
    final_dict = {
        col: [l[col_idx[0] : col_idx[1]].strip() for l in
              text_table.splitlines()[1:]]
        for col, col_idx in zip(columns, cols_idxs)
    }
    return final_dict


class NMInfo():
    def __init__(self, exclude=[]):
        P = subprocess.Popen('nmcli monitor', text=True, shell=True,
                             stdout=subprocess.PIPE)
        self.updater = P.stdout
        self.exclude = exclude
        self.wait_time = 600

    def get_devices_status(self):
        devices_table = cmd_output('nmcli device')
        return read_fwf(devices_table)

    def get_wifi_networks(self, ifname):
        cmd = f'nmcli dev wifi list --rescan auto ifname {ifname}'
        wifi_aps = cmd_output(cmd)
        return read_fwf(wifi_aps)

    def output(self):
        try:
            devs = self.get_devices_status()
        except IndexError:
            return '...'
        output = '%{A:NM_MENU:}'
        for dev_name, dev_type, dev_state, dev_con in zip(
                devs['DEVICE'], devs['TYPE'], devs['STATE'], devs['CONNECTION']):
            if dev_name == 'lo' or dev_name in self.exclude:
                continue
            output += '%{A3:NMIFINFO_'+dev_name+':}'
            if dev_type == 'wifi' and dev_state == 'connected':
                # wifi_nets = self.get_wifi_networks(dev_name)
                # bars = wifi_nets['BARS'][wifi_nets['SSID'].index(dev_con)]
                output += '%{A2:WIFIQR_'+dev_name+':}'
                output += ficon('\uf1eb', cdict['green'])
                output += '%{A2}'
                # output += bars
            elif dev_type == 'wifi' and dev_state == 'disconnected':
                output += ficon('\uf1eb', cdict['orange'])
            elif dev_type == 'wifi' and dev_state in ['disabled', 'unavailable']:
                output += ficon('\uf1eb', cdict['red'])
            elif dev_type == 'wifi' and dev_state == 'unmanaged':
                output += ficon('\uf1eb', cdict['dimmed'])
            elif dev_type == 'ethernet' and dev_state == 'connected':
                output += ficon('\uf796', cdict['green'])
            elif dev_type == 'ethernet' and dev_state == 'connecting':
                output += ficon('\uf796', cdict['light_yellow'])
            elif dev_type == 'ethernet' and dev_state in ['unavailable', 'disconnected']:
                output += ficon('\uf796', cdict['orange'])
            elif dev_type == 'ethernet' and dev_state == 'unmanaged':
                output += ficon('\uf796', cdict['dimmed'])
            output += '%{A3}'
        return output + '%{A}'

    def command(self, event):
        if event == 'NM_MENU':
            subprocess.Popen(
                'rofi -show nmcli -modi nmcli:~/Scripts/RofiMenus/nmcli-menu.py',
                shell=True, text=True)
        elif event.startswith('WIFIQR'):
            ifname = event.split("_")[-1]
            subprocess.Popen(f'konsole --hold -e nmcli dev wifi show ifname {ifname}', shell=True, text=True)
        elif event.startswith('NMIFINFO'):
            ifname = event.split("_")[-1]
            dev_info = cmd_output(f'nmcli dev show {ifname}')
            subprocess.Popen(f'dunstify -a "NetworkManager" "{ifname.upper()} INFO" "{dev_info}"',
                             shell=True, text=True)


class PacmanUpdates():
    def __init__(self):
        self.wait_time = 600  # for first update only
        self.updater = None
        self.cache = 'N/A'

    def pacman_thread(self):
        aur_packs = cmd_output('yay -Qu | tee /tmp/aurupdates | wc -l')
        official = cmd_output('pacman -Qu | tee /tmp/pacmanupdates | wc -l')
        all = int(aur_packs) + int(official)
        self.cache = all
        if self.wait_time == 600:
            self.wait_time = 1
            time.sleep(1)
            self.wait_time = 3600

    def output(self):
        T = threading.Thread(target=self.pacman_thread)
        T.start()
        return ('%{A:PACMAN:}'+ficon('\uf466', cdict['l_yellow']) +
                f'{self.cache}%{{A}}')

    def command(self, event):
        if event.startswith('PACMAN'):
            subprocess.Popen('konsole -e pikaur -Su', text=True, shell=True)


class DiskUsage():
    def __init__(self, mount_point, icon=None):
        self.wait_time = 9000
        self.updater = None
        self.mount_point = mount_point
        self.icon = icon

    def output(self):
        df_out = cmd_output('df')
        if df_out:
            used_mnt = [line for line in df_out.split('\n')
                        if line.endswith(self.mount_point)]
            if used_mnt:
                icon = ficon(self.icon)
                return icon + used_mnt[0].split()[-2]
        return 'N/A'

    def command(self, event):
        pass


class RamUsage():
    def __init__(self, percent=False):
        self.wait_time = 30
        self.updater = None
        self.icon = '\uf538'
        self.percent = percent

    def output(self):
        ram = cmd_output('vmstat -s').split('\n')
        used = int(ram[1].strip().split()[0])/(1024**2)
        total = int(ram[0].strip().split()[0])/(1024**2)
        percent = f'{used/total:0>3.0%}%'
        if self.percent and used/total > 0.85:
            return ('%{F'+cdict['red']+'}'+ficon(self.icon)+f'{percent}'+'%{F-}')
        elif self.percent:
            return ficon(self.icon, cdict['green'])+f'{percent}'
        elif not self.percent and used/total > 0.85:
            return ('%{F'+cdict['red']+'}%{T2}'
                    f'{self.icon}%{{T-}}{used:0.1f}G/{total:0.1f}G'+'%{F-}')
        else:
            return f'%{{F{cdict["green"]}}}%{{T2}}{self.icon}%{{T-}}%{{F-}}{used:0.1f}G/{total:0.1f}G'

    def command(self, event):
        pass


class CurrentWindow():
    # TODO
    def __init__(self):
        P = subprocess.Popen('bspc subscribe', text=True, shell=True,
                             stdout=subprocess.PIPE, encoding='UTF-8')
        self.updater = P.stdout
        self.wait_time = 60

    def output(self):
        win_title = cmd_output('bspc query -N -n | xtitle')
        win_title = win_title+' '*(28-len(win_title)) if len(win_title) < 28 else win_title[:25]+'...'
        return win_title

    def command(self, event):
        pass


class Battery():
    def __init__(self):
        self.wait_time = 60
        self.updater = None
        self.icons = {5: '\uf244', 25: '\uf243', 50: '\uf242',
                      75: '\uf241', 100: '\uf240'}

    def output(self):
        battery = cmd_output('acpi --battery')
        if battery != '':
            charging = 'Charging' in battery
            battery = battery.split(': ')[1].split(', ')[1]
            bat_vlu = int(battery.rstrip('%'))
            # literal % should be passed as %% to lemonbar
            battery += '%'
            icon = [v for k, v in self.icons.items() if k >= bat_vlu][0]
            if bat_vlu <= 5:
                return '%{F'+cdict['red']+'}'+ficon(icon)+battery+'%{F-}'
            elif bat_vlu == 100:
                return '%{F'+cdict["green"]+'}'+ficon(icon)+battery+'%{F-}'
            elif charging:
                return '%{F'+cdict["l_yellow"]+'}'+ficon(icon)+battery+'%{F-}'
            else:
                return ficon(icon)+battery

    def command(self, event):
        pass


class QtileWorkspaces():
    try:
        from libqtile.command_client import InteractiveCommandClient as ICC
        from libqtile.ipc import IPCError
        IMPORTED = True
    except ImportError:
        IMPORTED = False
    def __init__(self):
        if not IMPORTED:
            self.ouput = lambda: ""
            return
        P = subprocess.Popen(
            os.path.expanduser('~/.config/bspwm/bar/qtile_signal_handler.py'),
            text=True, stdout=subprocess.PIPE, encoding='UTF-8')
        self.updater = P.stdout
        self.wait_time = 60
        self.icc = self.ICC()

    def output(self):
        try:
            return self.__output()
        except self.IPCError:
            while True:
                try:
                    time.sleep(1)
                    del self.icc
                    self.icc = self.ICC()
                    return self.__output()
                except ConnectionRefusedError:
                    continue

    def __output(self):
        grps = self.icc.groups()
        all_workspaces = [grp for grp in grps.keys() if not grp == 'scratchpad']
        just_len = len(max(all_workspaces, key=len))
        empty_workspaces = [grp for grp in grps.keys()
                            if not grp == 'scratchpad' and len(grps[grp]['windows']) == 0]
        current = self.icc.group.info()['name']

        pre1 = '%{A:QTILE_WIDGETdesk'
        pre2 = '%{A4:QTILE_WIDGETnext:}'
        pre3 = '%{A5:QTILE_WIDGETprev:}'
        formatted_ws = []
        for w in all_workspaces:
            wor = f' {w} '
            # wor = '%{T3}'+w.center(just_len+2)+'%{T1}'
            # wor = w
            if w == current:
                formatted_ws.append('%{R}'+wor+'%{R}')
            # elif w in urgent:
            #     formatted_ws.append(
            #         pre1+w+':}'+'%{U'+cdict['red']+'}%{+o}'+wor+'%{-o}%{U-}%{A}')
            elif w in empty_workspaces:
                formatted_ws.append(
                    pre1+w+':}'+'%{U'+cdict['cyan']+'}%{+o}'+wor+'%{-o}%{U-}%{A}')
            else:
                formatted_ws.append(
                    pre1+w+':}'+wor+'%{A}')
        return pre2+pre3+''.join(formatted_ws)+'%{A5}%{A4}'

    def command(self, event):
        if event.startswith('QTILE_WIDGETdesk'):
            w = event.strip()[16:]
            self.icc.screen.toggle_group(w)
        elif event in ['QTILE_WIDGETnext', 'QTILE_WIDGETprev']:
            if event.endswith('next'):
                self.icc.screen.next_group()
            if event.endswith('prev'):
                self.icc.screen.prev_group()


class HerbstluftwmWorkspaces():
    def __init__(self, icons_dict=None, numbers=False):
        P = subprocess.Popen(
            "herbstclient --idle 'tag_changed|tag_flags'",
            text=True, shell=True, stdout=subprocess.PIPE, encoding='UTF-8')
        self.updater = P.stdout
        self.wait_time = 60
        self.icns = icons_dict
        self.numbers = numbers

    def output(self):
        tags_status = cmd_output('herbstclient tag_status')
        pre1 = '%{A:HERBST_WIDGETdesk'
        pre2 = '%{A4:HERBST_WIDGETnext:}'
        pre3 = '%{A5:HERBST_WIDGETprev:}'
        sfx = '%{-o}%{U-}%{A}'
        format_dict = {':': None, '-': 'orange', '.': 'cyan',
                       '!': 'red', '#': 'current'}
        formatted_ws = []
        for i, w in enumerate(tags_status.split('\t')):
            wor = f' {w[1:]} '
            if self.icns and not self.numbers:
                wor = ficon(self.icns.get(w[1:], ''), beforepad=5, afterpad=5)
            elif self.numbers:
                wor = f' {i} ' 
            clr = format_dict.get(w[0], None)
            if clr:
                if clr == 'current':
                    formatted_ws.append('%{R}'+wor+'%{R}')
                    continue
                formatted_ws.append(
                    pre1+w[1:]+':}'+(('%{U'+cdict[clr]+'}') if clr else '')+
                    '%{+o}'+wor+sfx)
            else:
                formatted_ws.append(pre1+w[1:]+':}'+wor+'%{A}')
                
        return pre2+pre3+''.join(formatted_ws)+'%{A5}%{A4}'

    def command(self, event):
        if event.startswith('HERBST_WIDGETdesk'):
            w = event.strip()[17:]
            print(w)
            subprocess.Popen(f'herbstclient use {w}', text=True, shell=True)
        elif event in ['HERBST_WIDGETnext', 'HERBST_WIDGETprev']:
            event = "-1" if event[-4:] == 'prev' else "+1"
            subprocess.Popen(f'herbstclient use_index {event}',
                             text=True, shell=True)


class HerbstluftwmWorkspacesDots():
    def __init__(self):
        P = subprocess.Popen("herbstclient --idle 'focus_changed|tag_changed'", text=True, shell=True,
                             stdout=subprocess.PIPE, encoding='UTF-8')
        self.updater = P.stdout
        self.wait_time = 60

    def output(self):
        wor_count = int(cmd_output('herbstclient attr tags.count'))
        all_workspaces = [cmd_output(f'herbstclient attr tags.{i}.name')
                          for i in range(wor_count)]
        just_len = len(max(all_workspaces, key=len))
        empty_workspaces, urgent_tags = [], []
        for desk in all_workspaces:
            wins_count = cmd_output(f"herbstclient attr tags.by-name.{desk}.client_count")
            urgent_count = cmd_output(f'herbstclient attr tags.by-name.{desk}.urgent_count')
            urgent_count = 0 if urgent_count == '' else urgent_count
            if int(wins_count) == 0:
                empty_workspaces.append(desk)
            if int(urgent_count) != 0:
                urgent_tags.append(desk)
        current = cmd_output('herbstclient attr tags.focus.name').strip()

        pre1 = '%{A:HERBST_WIDGETdesk'
        pre2 = '%{A4:HERBST_WIDGETnext:}'
        pre3 = '%{A5:HERBST_WIDGETprev:}'
        formatted_ws = []
        for w in all_workspaces:
            wor = ''
            suffix = '%{T-}%{-o}%{U-}%{A}'
            spacing = str(3*GDKSCALE)
            other_side = str(int(spacing)-9*GDKSCALE)
            if w == current:
                if w in empty_workspaces:
                    wor = ''
                formatted_ws.append(
                    '%{R}%{O'+spacing+'}%{T2}'+wor+'%{T-}%{R}'+'%{O'+
                    other_side+'}')
            elif w in urgent_tags:
                formatted_ws.append(
                    pre1+w+':}'+'%{U'+cdict['red']+'}%{+o}%{O'+
                    spacing+'}%{T2}'+wor+suffix+'%{O'+other_side+'}')
                # 7 is the fixed spacing afterward this symbol that needs to be removed
            elif w in empty_workspaces:
                wor = ''
                formatted_ws.append(
                    pre1+w+':}'+'%{O'+spacing+'}%{T2}'+wor+'%{T-}%{A}'+'%{O'+
                    other_side+'}')
            else:
                formatted_ws.append(
                    pre1+w+':}%{O'+spacing+'}%{T2}'+wor+'%{T-}%{A}'+
                    '%{O'+other_side+'}')

        return pre2+pre3+''.join(formatted_ws)+'%{A5}%{A4}'

    def command(self, event):
        if event.startswith('HERBST_WIDGETdesk'):
            w = event.strip()[17:]
            print(w)
            subprocess.Popen(f'herbstclient use {w}', text=True, shell=True)
        elif event in ['HERBST_WIDGETnext', 'HERBST_WIDGETprev']:
            event = "-1" if event[-4:] == 'prev' else "+1"
            subprocess.Popen(f'herbstclient use_index {event}',
                             text=True, shell=True)


class BspwmWorkspaces():
    def __init__(self):
        P = subprocess.Popen('bspc subscribe', text=True, shell=True,
                             stdout=subprocess.PIPE, encoding='UTF-8')
        self.updater = P.stdout
        self.wait_time = 60

    def output(self):
        all_workspaces = cmd_output(
            'bspc query -D --names').strip().split('\n')
        just_len = len(max(all_workspaces, key=len))
        empty_workspaces = []
        for desk in all_workspaces:
            wins = cmd_output(f"bspc query -N -d {desk}")
            wins = [] if wins == '' else wins
            if len(wins) == 0:
                empty_workspaces.append(desk)
        current = cmd_output('bspc query -D -d --names').strip()
        try:
            urgent = cmd_output(
                    'bspc query -D -d .urgent --names').strip().split('\n')
        except subprocess.CalledProcessError:
            urgent = []

        pre1 = '%{A:BSPWM_WIDGETdesk'
        pre2 = '%{A4:BSPWM_WIDGETnext:}'
        pre3 = '%{A5:BSPWM_WIDGETprev:}'
        formatted_ws = []
        for w in all_workspaces:
            wor = f' {w} '
            # wor = '%{T3}'+w.center(just_len+2)+'%{T1}'
            # wor = w
            if w == current:
                formatted_ws.append('%{R}'+wor+'%{R}')
            elif w in urgent:
                formatted_ws.append(
                    pre1+w+':}'+'%{U'+cdict['red']+'}%{+o}'+wor+'%{-o}%{U-}%{A}')
            elif w in empty_workspaces:
                formatted_ws.append(
                    pre1+w+':}'+'%{U'+cdict['cyan']+'}%{+o}'+wor+'%{-o}%{U-}%{A}')
            else:
                formatted_ws.append(
                    pre1+w+':}'+wor+'%{A}')

        return pre2+pre3+''.join(formatted_ws)+'%{A5}%{A4}'

    def command(self, event):
        if event.startswith('BSPWM_WIDGETdesk'):
            w = event.strip()[16:]
            print(w)
            subprocess.Popen(f'bspc desktop -f {w}', text=True, shell=True)
        elif event in ['BSPWM_WIDGETnext', 'BSPWM_WIDGETprev']:
            subprocess.Popen(f'bspc desktop -f {event[-4:]}',
                             text=True, shell=True)


class XAutoLocker():
    def __init__(self):
        self.wait_time = 6000
        self.updater = None
        self.coffee = '\uf0f4'
        self.lock = '\uf023'
        self.enabled = False

    def output(self):
        coffee = ficon(self.coffee, cdict['green'] if self.enabled else cdict['dimmed'])
        lock = ficon(self.lock, cdict['l_yellow'])
        return '%{A:XAUTOLOCK:}'+coffee+'%{A}%{A:XAUTOLOCKNOW:}'+lock+'%{A}'

    def command(self, event):
        if event == 'XAUTOLOCK':
            # used enable and disable instead of toggle, just incase state has
            # been toggled outside of the bar process, xautolock has no way of
            # querying its current state
            subprocess.Popen(f'xautolock -{"enable" if self.enabled else "disable"}'.split())
            subprocess.Popen(f'xset {"-" if self.enabled else "+"}dpms'.split())
            self.enabled = not self.enabled
            return True
        elif event == 'XAUTOLOCKNOW':
            subprocess.Popen('xautolock -locknow'.split())
        

class TimeDate():
    def __init__(self, timeformat=None):
        self.wait_time = 60
        self.updater = None
        self.timeformat = timeformat
        self.month_year = (datetime.date.today().year,
                            datetime.date.today().month)
        self.txt_cal = calendar.TextCalendar()

    def output(self):
        clock_faces = {(0, 0): "🕛", (1, 0): "🕐", (2, 0): "🕑",
                       (3, 0): "🕒", (4, 0): "🕓", (5, 0): "🕔",
                       (6, 0): "🕕", (7, 0): "🕖", (8, 0): "🕗",
                       (9, 0): "🕘", (10, 0): "🕙", (11, 0): "🕚",
                       (0, 30): "🕧", (1, 30): "🕜", (2, 30): "🕝",
                       (3, 30): "🕞", (4, 30): "🕟", (5, 30): "🕠",
                       (6, 30): "🕡", (7, 30): "🕢", (8, 30): "🕣",
                       (9, 30): "🕤", (10, 30): "🕥", (11, 30): "🕦"}
        clock_faces = {0: '\ue381', 1: '\ue382', 2: '\ue383', 3: '\ue384', 4: '\ue385',
                      5: '\ue386', 6: '\ue387', 7: '\ue388', 8: '\ue389', 9: '\ue38a',
                      10: '\ue38b', 11: '\ue38c'}
        now = datetime.datetime.now()
        current_face = now.hour if now.hour < 12 else now.hour-12
        # 0 if now.minute < 30 else 30)
        date_time = ('%{A:currentcal:}%{A4:nextcal:}%{A5:prevcal:}' +
                     ficon('\uf073', cdict['orange'])+'%{A5}%{A4}%{A}' +
                     datetime.datetime.strftime(now, '%a %d-%b,%y %H:%M'))
        return date_time+ficon(' \uf017')

    def command(self, event):
        if event == 'currentcal':
            self.month_year = (datetime.date.today().year,
                               datetime.date.today().month)
        elif event == 'nextcal':
            if self.month_year[1] == 12:
                self.month_year = (self.month_year[0]+1, 1)
            else:
                self.month_year = (self.month_year[0], self.month_year[1]+1)
        elif event == 'prevcal':
            if self.month_year[1] == 1:
                self.month_year = (self.month_year[0]-1, 12)
            else:
                self.month_year = (self.month_year[0], self.month_year[1]-1)
        if event in ['currentcal', 'nextcal', 'prevcal']:
            cal = self.txt_cal.formatmonth(*self.month_year)
            cal_hdr = cal.splitlines()[0]
            cal_body = '\n'.join(cal.splitlines()[1:])
            subprocess.Popen(
                (f'dunstify -i office-calendar "{cal_hdr}" "{cal_body}" '
                 '-r 000010 -h "string:desktop-entry:calendar_popup"'),
                shell=True, text=True)


class RandomNum():
    def __init__(self):
        self.wait_time = 60
        self.updater = None
        self.event = 'Update Num'

    def output(self):
        num = random.randint(0, 100)
        color = cdict['green'] if num == 100 else '-'
        return f'%{{A:Update Num:}}%{{F{color}}}' + str(num)+'%{F-}%{A}'

    def command(self, event):
        if event == 'Update Num':
            return True

