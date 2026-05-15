require("lz.n").load({
	"blink.cmp",
	event = "InsertEnter",
	load = function(name)
		vim.cmd.packadd(name)
		vim.cmd.packadd("friendly-snippets")
		vim.cmd.packadd("blink.compat")
		vim.cmd.packadd("vim-dadbod-completion")
	end,
	after = function()
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
				default = { "lsp", "path", "snippets", "buffer" },
				per_filetype = { sql = { "dadbod" } },
				providers = {
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
