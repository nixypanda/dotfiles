local wk = require("which-key")

local mappings = {
	{ "<leader>b", group = "buffers" },
	{ "<leader>c", group = "quickfix" },
	{ "<leader>D", group = "database" },
	{ "<leader>d", group = "debug" },
	{ "<leader>e", group = "explorer (nvim-tree)" },
	{ "<leader>g", group = "git" },
	{ "<leader>l", group = "code (lsp)" },
	{ "<leader>o", group = "codect" },
	{ "<leader>p", group = "code (language specific)" },
	{ "<leader>s", group = "search" },
	{ "<leader>t", group = "test (neotest)" },
	{ "<leader>u", group = "utilities" },
	{ "<leader>w", group = "layout (windows)" },
}

wk.add(mappings)
