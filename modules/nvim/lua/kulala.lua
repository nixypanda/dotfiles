require("lz.n").load({
	"kulala.nvim",
	keys = {
		{ "<leader>hl", "<cmd>lua require('kulala').run()<cr>", desc = "Run kulala call" },
	},
	ft = { "http" },
	after = function()
		require("kulala").setup({
			global_keymaps = false,
			global_keymaps_prefix = "<leader>R",
			kulala_keymaps_prefix = "",
			lsp = {
				enable = true,
				keymaps = false, -- disabled by default, as Kulala relies on default Neovim LSP keymaps
				formatter = {
					sort = { -- enable/disable alphabetical sorting in request body
						metadata = true,
						variables = true,
						commands = true,
						json = false,
					},
				},
				on_attach = require("common").lsp_on_attach,
			},
		})
	end,
})
