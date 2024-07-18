-- Completion (default setting)
vim.o.completeopt = "menuone,noselect"

---@param fpattern string filename pattern
---@return integer
--HACK:this is needed only for cmp sources because they (incorrectly) use "after/plugin"
--folder to register themselves.
local function load_after_plugin(fpattern)
	--- based on: https://github.com/echasnovski/mini.deps/blob/main/lua/mini/deps.lua
	-- Execute 'after/' scripts if not during startup (when they will be sourced
	-- automatically), as `:packadd` only sources plain 'plugin/' files.
	-- See https://github.com/vim/vim/issues/1994.
	-- Deliberately do so after executing all currently known 'plugin/' files.
	local should_load_after_dir = vim.v.vim_did_enter == 1 and vim.o.loadplugins
	if not should_load_after_dir then return 0 end
	-- NOTE: This sources first lua and then vim, not how it is done during
	-- startup (`:h loadplugins`) for speed (one `glob()` instead of two).
	local after_paths = vim.api.nvim_get_runtime_file("after/plugin/" .. fpattern, true)
	vim.tbl_map(function(f) vim.cmd("source " .. vim.fn.fnameescape(f)) end, after_paths)
	return #after_paths
end

require("lz.n").load({
	"nvim-cmp",
	event = "InsertEnter",
	load = function(name)
		vim.cmd.packadd(name)
		-- vim.cmd.packadd("lspkind.nvim")
		vim.cmd.packadd("crates.nvim")
		vim.cmd.packadd("vim-dadbod-completion")
		vim.cmd.packadd("codeium.nvim")
	end,
	after = function()
		local source_plugins = {
			"cmp_luasnip",
			"cmp-buffer",
			"cmp-path",
			"cmp-calc",
			"cmp-nvim-lsp",
			"nvim_lua",
		}
		for _, plug in ipairs(source_plugins) do
			vim.cmd.packadd(plug)
		end

		local after_sourced = load_after_plugin("cmp*.lua")
		if after_sourced ~= #source_plugins then
			vim.notify(
				"expected " .. #source_plugins .. " cmp source after/plugin sources, but got " .. after_sourced,
				vim.log.levels.WARN
			)
		end

		-- Normal config --

		local cmp = require("cmp")

		local has_words_before = function()
			unpack = unpack or table.unpack
			local line, col = unpack(vim.api.nvim_win_get_cursor(0))
			return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
		end

		require("codeium").setup({
			tools = {
				-- injected from nix
				language_server = codeium_language_server,
			},
		})

		cmp.setup({
			snippet = {
				expand = function(args) require("luasnip").lsp_expand(args.body) end,
			},
			formatting = {
				format = require("lspkind").cmp_format({
					mode = "symbol_text",
					maxwidth = 50,
					symbol_map = { Codeium = "" },
				}),
			},
			mapping = {
				["<C-d>"] = cmp.mapping(cmp.mapping.scroll_docs(-4), { "i", "c" }),
				["<C-f>"] = cmp.mapping(cmp.mapping.scroll_docs(4), { "i", "c" }),
				["<C-Space>"] = cmp.mapping(cmp.mapping.complete(), { "i", "c" }),
				-- If you want to remove the default `<C-y>` mapping, You can specify
				-- `cmp.config.disable` value.
				["<C-y>"] = cmp.config.disable,
				["<C-e>"] = cmp.mapping({ i = cmp.mapping.abort(), c = cmp.mapping.close() }),
				["<CR>"] = cmp.mapping.confirm({ select = false }),
				["<Tab>"] = cmp.mapping(function(fallback)
					if cmp.visible() then
						cmp.select_next_item()
					elseif require("luasnip").expand_or_locally_jumpable() then
						require("luasnip").expand_or_jump()
					elseif has_words_before() then
						cmp.complete()
					else
						fallback()
					end
				end, { "i", "s" }),

				["<S-Tab>"] = cmp.mapping(function(fallback)
					if cmp.visible() then
						cmp.select_prev_item()
					elseif require("luasnip").jumpable(-1) then
						require("luasnip").jump(-1)
					else
						fallback()
					end
				end, { "i", "s" }),
			},
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
		cmp.setup.filetype("lua", { sources = { { name = "nvim_lua" } } })
		cmp.setup.filetype({ "sql", "mysql", "plsql" }, { sources = { { name = "vim-dadbod-completion" } } })
	end,
})
