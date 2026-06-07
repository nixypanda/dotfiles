require("nvim-treesitter").setup({
	ensure_installed = {
		-- This needs to be empty otherwise treesitter complains about
		-- directory being not being writable. All the installation of the
		-- parsers is done declaratively into an immutable location using nix,
		-- so we don't really need to specify anything there.
		-- https://github.com/NixOS/nixpkgs/issues/189838
	},
	auto_install = false,
	highlight = {
		enable = true,
		additional_vim_regex_highlighting = false,
	},
	indent = { enable = true },
	incremental_selection = {
		enable = true,
	},
})

local function register_treesitter_filetype(filetype)
	local group = vim.api.nvim_create_augroup("FUTreesitter_" .. filetype, { clear = true })

	vim.api.nvim_create_autocmd("FileType", {
		group = group,
		pattern = filetype,
		callback = function() vim.treesitter.start() end,
	})
end

vim.treesitter.language.register("kulala_http", { "http" })

register_treesitter_filetype("bash")
register_treesitter_filetype("dockerfile")
register_treesitter_filetype("elm")
register_treesitter_filetype("haskell")
register_treesitter_filetype("json")
register_treesitter_filetype("kdl")
register_treesitter_filetype("lua")
register_treesitter_filetype("markdown")
register_treesitter_filetype("markdown-inline")
register_treesitter_filetype("nix")
register_treesitter_filetype("nu")
register_treesitter_filetype("python")
register_treesitter_filetype("regex")
register_treesitter_filetype("rust")
register_treesitter_filetype("sql")
register_treesitter_filetype("toml")
register_treesitter_filetype("vimdoc")
register_treesitter_filetype("yaml")
register_treesitter_filetype("http")
register_treesitter_filetype("ledger")
