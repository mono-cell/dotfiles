#!/usr/bin/env python

import sys
from start_lemonbar import create_powerline, lemonbar_below_xfcepanel, HOSTNAME

from colors import modus_vivendi as cdict
from lemonbar_script import MainLoop
from bar_modules import HerbstluftwmWorkspacesDots,\
    PacmanUpdates, NetworkTraffic, DiskUsage,\
    CPUTemp, RamUsage, Battery, RandomNum, KeyboardLayout, TimeDate,\
    HerbstluftwmWorkspaces,\
    NMInfo, ficon, XAutoLocker, DarkLightSwitcher


if __name__ == '__main__':

    bgs = ['#282A36', '#000000', '#373844', '#1E2029', '#6272a4']

    tags_icns = {
        'WEB': '\uf0ac', 'DEV': '\uf5fc', 'TERM': '\uf120', 'DOCS': '\uf02d',
        'GIMP': '\uf1fc', 'READ': '\uf518', 'AGENDA': '\uf274',
        'DOWN': '\uf019', 'CHAT': '\uf086', 'GAME': '\uf11b'}

    modules = [
        HerbstluftwmWorkspaces(tags_icns),
        PacmanUpdates(),
       '%{r}',
       Battery(),
       TimeDate()
    ]
    # seps = [ficon('\ue0b0'), ficon('\ue0b1', beforepad=5), ficon('\ue0b2', afterpad=-0.5),
    #         ficon('\ue0b3', beforepad=5)]
    # seps = [' ', ficon('\ue0b1', beforepad=5), ' ', ficon('\ue0b3', beforepad=5)]
    # create_powerline(modules, bgs, seps=seps)

    main_loop = MainLoop(modules, sep='', bg='#282a36', fg='#F8F8F2')
    main_loop.start_lemonbar()
    lemonbar_below_xfcepanel()
    main_loop.start_loop()
