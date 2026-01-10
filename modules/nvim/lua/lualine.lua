vim.api.nvim_set_option_value("laststatus", 3, {})

require("lualine").setup({
	options = {
		disabled_filetypes = { "NvimTree" },
		iconsEnabled = true,
		component_separators = "",
		section_separators = { left = "", right = "" },
	},
	sections = {
		lualine_a = { "mode" },
		lualine_b = { { "filename", path = 4 }, "branch" },
		lualine_c = {
			{
				"diagnostics",
				sources = { "nvim_diagnostic" },
				symbols = { error = "☠ ", warn = " ", info = "", hint = "☛ " },
			},
		},
		lualine_x = { "diff" },
		lualine_y = { "filetype", "progress" },
		lualine_z = { "location" },
	},
})
