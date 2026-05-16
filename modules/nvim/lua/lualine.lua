vim.api.nvim_set_option_value("laststatus", 3, {})

local nvim_tree_shift = {
	function()
		local ok, api = pcall(require, "nvim-tree.api")
		if not ok then return "" end

		local winid = api.tree.winid() or 0
		if winid == 0 then return "" end

		return string.rep(" ", vim.api.nvim_win_get_width(winid) - 2)
	end,
	cond = function()
		local ok, api = pcall(require, "nvim-tree.api")
		return ok and api.tree.is_visible()
	end,
	color = "NvimTreeNormal",
}

require("lualine").setup({
	options = {
		disabled_filetypes = { "NvimTree" },
		icons_enabled = true,
		component_separators = "",
		section_separators = { left = "", right = "" },
	},
	sections = {
		lualine_a = { nvim_tree_shift, "mode" },
		lualine_b = { { "filename", path = 4 }, "branch" },
		lualine_c = {
			{
				"diagnostics",
				sources = { "nvim_diagnostic" },
				symbols = { error = "☠ ", warn = " ", info = "", hint = "☛ " },
			},
		},
		lualine_x = { "diff" },
		lualine_y = { "lsp_status", "filetype" },
		lualine_z = { "location" },
	},
})
