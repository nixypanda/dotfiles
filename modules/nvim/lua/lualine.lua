vim.api.nvim_set_option("laststatus", 3)

require("lualine").setup({
	options = {
		disabled_filetypes = { "NvimTree" },
		iconsEnabled = true,
	},
	sections = {
		lualine_a = { { "mode", upper = true } },
		lualine_b = { "filename" },
		lualine_c = {
			{
				"diagnostics",
				sources = { "nvim_diagnostic" },
				symbols = { error = "☠ ", warn = " ", info = "", hint = "☛ " },
			},
		},
		lualine_x = { "diff", "branch" },
		lualine_y = { "progress" },
		lualine_z = { "location" },
	},
})
