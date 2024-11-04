local wk = require("which-key")
wk.setup({})

local mappings = {
	{ "<leader>b", group = "buffers" },
	{ "<leader>c", group = "code (coverage)" },
	{ "<leader>D", group = "database" },
	{ "<leader>d", group = "debug" },
	{ "<leader>e", group = "explorer (neo-tree)" },
	{ "<leader>f", group = "code (formatting)" },
	{ "<leader>g", group = "git" },
	{ "<leader>gc", group = "conflict" },
	{ "<leader>l", group = "code (lsp)" },
	{ "<leader>p", group = "code (language specific)" },
	{ "<leader>s", group = "search" },
	{ "<leader>t", group = "test (neotest)" },
	{ "<leader>u", group = "utilities" },
}

wk.add(mappings)
