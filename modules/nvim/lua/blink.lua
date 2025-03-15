local lz = require("lz.n")

lz.load({
	"blink.cmp",
	event = "InsertEnter",
	load = function(name)
		vim.cmd.packadd(name)
		vim.cmd.packadd("friendly-snippets")
		vim.cmd.packadd("blink.compat")
		vim.cmd.packadd("codeium.nvim")
	end,
	after = function()
		require("codeium").setup({
			tools = {
				language_server = codeium_language_server_bin,
			},
		})
		require("blink.cmp").setup({
			appearance = {
				use_nvim_cmp_as_default = true,
				nerd_font_variant = "mono",
				kind_icons = {
					codeium = "",
				},
			},
			keymap = {
				preset = "default",
				["<Tab>"] = { "select_next", "snippet_forward", "fallback" },
				["<S-Tab>"] = { "select_prev", "snippet_backward", "fallback" },
			},
			sources = {
				default = { "lsp", "path", "codeium", "snippets", "buffer", "dadbod" },
				providers = {
					codeium = {
						name = "codeium", -- IMPORTANT: use the same name as you would for nvim-cmp
						module = "blink.compat.source",
						score_offset = 5,
						async = true,
						max_items = 3,
						min_keyword_length = 3,
					},
					dadbod = { name = "Dadbod", module = "vim_dadbod_completion.blink" },
				},
			},
			completion = {
				menu = {
					border = "rounded",
					max_height = 25,
					draw = {
						columns = {
							{ "label", "label_description", gap = 1 },
							{ "kind_icon", "kind", gap = 1 },
						},
					},
				},
				documentation = {
					auto_show = true,
					auto_show_delay_ms = 0,
					window = { border = "rounded" },
				},
			},
			signature = { window = { border = "single" } },
			fuzzy = {
				prebuilt_binaries = {
					download = false,
				},
			},
		})
	end,
})
