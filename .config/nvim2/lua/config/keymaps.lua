-----------------------------------------------------------
-- Define keymaps of Neovim and installed plugins.
-----------------------------------------------------------

local function map(mode, lhs, rhs, opts)
	local options = { noremap = true, silent = true }
	if opts then
		options = vim.tbl_extend("force", options, opts)
	end
	vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

map("n", "<F2>", ":set invpaste paste?<CR>")
vim.opt.pastetoggle = "<F2>"

map("n", "<leader>tk", "<C-w>t<C-w>K") -- change vertical to horizontal
map("n", "<leader>th", "<C-w>t<C-w>H") -- change horizontal to vertical

map("n", "<C-h>", "<C-w>h")
map("n", "<C-j>", "<C-w>j")
map("n", "<C-k>", "<C-w>k")
map("n", "<C-l>", "<C-w>l")
map("n", "<Tab><Tab>", "<C-w>w")

-- Reload configuration without restart nvim
map("n", "<leader>r", ":so %<CR>")
map("n", "|", ":vsplit<CR>")
map("n", "<leader>w", ":w<CR>")
map("n", "<leader>c", ":close<CR>")
map("n", "<M-Tab>", ":tabNext<CR>")

-- NvimTree
map("n", "\\", ":NvimTreeToggle<CR>") -- open/close
map("n", "cc", ":ColorizerToggle<CR>") -- open/close
