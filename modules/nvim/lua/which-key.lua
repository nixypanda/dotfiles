local wk = require("which-key")
wk.setup({})

local mappings = {
	{ "<leader>D", group = "database" },
	{ "<leader>b", group = "buffers" },
	{ "<leader>d", group = "debug" },
	{ "<leader>e", group = "explorer" },
	{ "<leader>g", group = "git" },
	{ "<leader>gc", group = "conflict" },
	{ "<leader>l", group = "code (lsp)" },
	{ "<leader>p", group = "code (language specific)" },
	{ "<leader>ph", group = "Haskell" },
	{ "<leader>pr", group = "Rust" },
	{ "<leader>s", group = "search" },
	{ "<leader>t", group = "test" },
	{ "<leader>u", group = "utilities" },
}

wk.add(mappings)
