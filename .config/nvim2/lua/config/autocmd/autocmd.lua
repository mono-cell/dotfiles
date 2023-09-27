local cmd            = vim.cmd
local create_augroup = vim.api.nvim_create_augroup
local create_autocmd = vim.api.nvim_create_autocmd

-- Load all needed modules.
require('config/autocmd/file')
require('config/autocmd/format')
--require('config/autocmd/miscellaneous')
--require('config/autocmd/lsp')

-----------------------
--- Define Augroups ---
-----------------------
-- Automatic commands to run on enter events.
local enter_group = create_augroup('enter_group', { clear = false })

create_autocmd('VimEnter', {
	callback = function()
		disable_automatic_line_commenting()
	end,
	group = enter_group })

-- Automatic commands to run on leave events.
local leave_group = create_augroup('leave_group', { clear = false })

create_autocmd('VimLeave', {
	callback = function()
		remove_unneeded_files()
	end,
	group = leave_group })

-- Automatic commands to run on read events.
local read_group = create_augroup('read_group', { clear = false })

create_autocmd('BufReadPost', {
	callback = function()
		save_cursor_position()
	end,
	group = read_group })

-- Automatic commands to run on write events.
local write_group = create_augroup('write_group', { clear = false })

create_autocmd('BufWritePre', {
	callback = function()
		remove_trailing_whitespace()
	end,
	group = write_group })

local miscellaneous = create_augroup('miscellaneous_group', { clear = false })

create_autocmd('BufEnter,BufWinEnter,WinEnter,CmdwinEnter', {
	callback = function()
		cmd([[ ColorizerAttachToBuffer ]])
	end,
	group = miscellaneous_group })
