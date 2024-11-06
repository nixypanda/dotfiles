---@diagnostic disable: undefined-field
local lz = require("lz.n")

lz.load({
	"nvim-cmp",
	event = "InsertEnter",
	load = function(name)
		vim.cmd.packadd(name)
		vim.cmd.packadd("LuaSnip")
		vim.cmd.packadd("lspkind.nvim")
		vim.cmd.packadd("friendly-snippets")

		-- cmp_source_path is injected from nix
		for plugin_name in pairs(cmp_source_path) do
			vim.cmd.packadd(plugin_name)
		end
	end,
	after = function()
		for plugin_name, plugin_path in pairs(cmp_source_path) do
			lz.trigger_load(plugin_name)
			require("rtp_nvim").source_after_plugin_dir(plugin_path)
		end

		local cmp = require("cmp")

		local luasnip = require("luasnip")
		require("luasnip.loaders.from_vscode").lazy_load()

		local key_mappings = cmp.mapping.preset.insert({
			["<C-d>"] = cmp.mapping.scroll_docs(-4),
			["<C-f>"] = cmp.mapping.scroll_docs(4),
			["<CR>"] = cmp.mapping(function(fallback)
				if cmp.visible() then
					if luasnip.expandable() then
						luasnip.expand()
					else
						cmp.confirm({ select = true })
					end
				else
					fallback()
				end
			end),
			["<Tab>"] = cmp.mapping(function(fallback)
				if cmp.visible() then
					cmp.select_next_item()
				elseif luasnip.locally_jumpable(1) then
					luasnip.jump(1)
				else
					fallback()
				end
			end, { "i", "s" }),

			["<S-Tab>"] = cmp.mapping(function(fallback)
				if cmp.visible() then
					cmp.select_prev_item()
				elseif luasnip.locally_jumpable(-1) then
					luasnip.jump(-1)
				else
					fallback()
				end
			end, { "i", "s" }),
		})

		local formatting = {
			format = require("lspkind").cmp_format({
				mode = "symbol_text",
				maxwidth = 50,
				symbol_map = {
					Codeium = "",
				},
			}),
		}

		cmp.setup({
			snippet = {
				expand = function(args) luasnip.lsp_expand(args.body) end,
			},
			formatting = formatting,
			mapping = key_mappings,
			sources = cmp.config.sources({
				{ name = "codeium", max_item_count = 3 },
				{ name = "nvim_lsp", max_item_count = 25 },
				{ name = "luasnip" },
				{ name = "calc" },
			}, {
				{ name = "path" },
				{ name = "buffer", max_item_count = 5, keyword_length = 3 },
			}),
			window = {
				completion = cmp.config.window.bordered(),
				documentation = cmp.config.window.bordered(),
			},
		})

		-- Use buffer source for `/` and `?`
		cmp.setup.cmdline({ "/", "?" }, {
			mapping = cmp.mapping.preset.cmdline(),
			sources = { { name = "buffer" } },
		})

		cmp.setup.filetype("Cargo.toml", { sources = { { name = "crates" } } })
		cmp.setup.filetype("lua", {
			sources = {
				{ name = "codeium", max_item_count = 3 },
				{ name = "nvim_lua" },
				{ name = "luasnip" },
				{ name = "buffer", max_item_count = 5, keyword_length = 3 },
			},
		})
		cmp.setup.filetype({ "sql", "mysql", "plsql" }, {
			sources = {
				{ name = "codeium", max_item_count = 3 },
				{ name = "vim-dadbod-completion" },
				{ name = "buffer" },
				{ name = "buffer", max_item_count = 5, keyword_length = 3 },
			},
		})

		vim.o.completeopt = "menuone,noselect"
	end,
})
