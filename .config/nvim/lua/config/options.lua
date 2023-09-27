local g = vim.g
local opt = vim.opt
-----------------------------------------------------------
-- General
-----------------------------------------------------------
opt.mouse = 'a'
opt.clipboard = 'unnamedplus'
opt.swapfile = false
opt.completeopt = 'menuone,noinsert,noselect'

opt.backup = false
opt.showmode = true
opt.timeoutlen = 500
opt.ttimeoutlen = 100
opt.wrap = false
-----------------------------------------------------------
-- Neovim UI
-----------------------------------------------------------
opt.number = true
opt.cursorline = true
opt.relativenumber = true
opt.showmatch = true
opt.splitright = true
opt.splitbelow = true
opt.ignorecase = true
opt.smartcase = true
opt.linebreak = true
opt.laststatus=3

opt.undofile = true
opt.undolevels = 100
-----------------------------------------------------------
-- Tabs, indent
-----------------------------------------------------------
opt.expandtab = true
opt.shiftwidth = 2
opt.tabstop = 2
opt.smartindent = true
-----------------------------------------------------------
-- Memory, CPU
-----------------------------------------------------------
opt.hidden = true
opt.history = 100
opt.lazyredraw = true
opt.synmaxcol = 240
opt.updatetime = 250
-----------------------------------------------------------
-- Startup
-----------------------------------------------------------
opt.shortmess:append "sI"

vim.cmd[[colorscheme tokyonight]]
