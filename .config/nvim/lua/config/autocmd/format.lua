local cmd = vim.cmd
local opt = vim.opt

function disable_automatic_line_commenting()
	-- Disable automatic commenting of new lines.
	opt.formatoptions = 'jql'
end

function remove_trailing_whitespace()
	-- Remove all trailing whitespace.
	cmd([[ :%s/\s\+$//e ]])
end
