require("lz.n").load({
	"blink.cmp",
	event = "InsertEnter",
	load = function(name)
		vim.cmd.packadd(name)
		vim.cmd.packadd("friendly-snippets")
		vim.cmd.packadd("blink.compat")
		vim.cmd.packadd("windsurf.nvim")
	end,
	after = function()
		require("codeium").setup({
			tools = {
				language_server = require("nix_injected").blink_codeium_language_server_bin,
			},
		})
		require("blink.cmp").setup({
			appearance = {
				nerd_font_variant = "mono",
			},
			enabled = function()
				return vim.bo.buftype ~= "prompt" and vim.b.completion ~= false and vim.bo.filetype ~= "NvimTree"
			end,
			keymap = {
				preset = "default",
				["<Tab>"] = { "select_next", "snippet_forward", "fallback" },
				["<S-Tab>"] = { "select_prev", "snippet_backward", "fallback" },
			},
			sources = {
				default = { "codeium", "lsp", "path", "snippets", "buffer" },
				per_filetype = { sql = { "dadbod" } },
				providers = {
					codeium = { name = "Codeium", module = "codeium.blink", async = true },
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
