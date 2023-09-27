return {
	{
		"nvim-telescope/telescope.nvim",
		cmd = "Telescope",
		version = false,
		lazy = true,
		dependencies = {
			"nvim-lua/plenary.nvim",
			"nvim-tree/nvim-web-devicons",
			{
				"nvim-telescope/telescope-file-browser.nvim",
				dependencies = { "nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim" },
			},
		},
		config = function()
			local telescope = require("telescope")
			telescope.setup({
				file_ignore_patterns = { "%.git/." },
				defaults = {
					previewer = false,
					-- hidden = true,
					prompt_prefix = "   ",
					file_ignore_patterns = { "node_modules", "package-lock.json" },
					initial_mode = "insert",
					select_strategy = "reset",
					sorting_strategy = "ascending",
					-- layout_strategy = "horizontal",
					layout_config = {
						--   width = 0.5,
						--   height = 0.4,
						prompt_position = "top",
						preview_cutoff = 120,
					},
				},
				pickers = {
					find_files = {
						-- theme = "dropdown",
						previewer = true,
						layout_config = {
							-- width = 0.5,
							height = 0.8,
							prompt_position = "top",
							preview_cutoff = 120,
						},
					},
					git_files = {
						previewer = true,
						-- theme = "dropdown",
						layout_config = {
							-- width = 0.5,
							height = 0.8,
							prompt_position = "top",
							preview_cutoff = 120,
						},
					},
					buffers = {
						previewer = true,
						-- theme = "dropdown",
						layout_config = {
							width = 0.5,
							height = 0.4,
							prompt_position = "top",
							preview_cutoff = 120,
						},
					},
					live_grep = {
						only_sort_text = true,
						previewer = true,
						layout_config = {
							horizontal = {
								width = 0.9,
								height = 0.75,
								preview_width = 0.6,
							},
						},
					},
					grep_string = {
						only_sort_text = true,
						previewer = true,
						layout_config = {
							horizontal = {
								width = 0.9,
								height = 0.75,
								preview_width = 0.6,
							},
						},
					},
				},
				extensions = {
					file_browser = {
						-- theme = "",
						previewer = true,
						-- disables netrw and use telescope-file-browser in its place
						hijack_netrw = true,
						-- mappings = {
						--   ["i"] = {
						--     -- your custom insert mode mappings
						--   },
						--   ["n"] = {
						--     -- your custom normal mode mappings
						--   },
						-- },
					},
				},
			})
			telescope.load_extension("file_browser")
		end,
	},
}
