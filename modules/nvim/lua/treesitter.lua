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

function register_ts_bullshit(filetype)
	local group = vim.api.nvim_create_augroup("FUTreesitter_" .. filetype, { clear = true })

	vim.api.nvim_create_autocmd("FileType", {
		group = group,
		pattern = filetype,
		callback = function() vim.treesitter.start() end,
	})
end

-- FIXME: broken
require("nvim-treesitter.parsers").kulala_http = {
	install_info = {
		path = require("nix_injected").treesitter_kulala_grammer_location,
	},
}
vim.treesitter.language.register("kulala_http", { "http" })

register_ts_bullshit("bash")
register_ts_bullshit("dockerfile")
register_ts_bullshit("haskell")
register_ts_bullshit("json")
register_ts_bullshit("kdl")
register_ts_bullshit("lua")
register_ts_bullshit("markdown")
register_ts_bullshit("markdown-inline")
register_ts_bullshit("nix")
register_ts_bullshit("nu")
register_ts_bullshit("python")
register_ts_bullshit("regex")
register_ts_bullshit("rust")
register_ts_bullshit("sql")
register_ts_bullshit("toml")
register_ts_bullshit("vimdoc")
register_ts_bullshit("yaml")
register_ts_bullshit("kulala_http")
