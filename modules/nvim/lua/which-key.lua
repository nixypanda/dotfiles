local wk = require("which-key")
wk.setup({})

local mappings = {
	{ "<leader>a", group = "AI" },
	{ "<leader>b", group = "buffers" },
	{ "<leader>c", group = "quickfix" },
	{ "<leader>D", group = "database" },
	{ "<leader>d", group = "debug" },
	{ "<leader>e", group = "explorer (nvim-tree)" },
	{ "<leader>f", group = "terminal (floating)" },
	{ "<leader>g", group = "git" },
	{ "<leader>k", group = "kulala (http)" },
	{ "<leader>l", group = "code (lsp)" },
	{ "<leader>m", group = "motions" },
	{ "<leader>p", group = "code (language specific)" },
	{ "<leader>s", group = "search" },
	{ "<leader>t", group = "test (neotest)" },
	{ "<leader>u", group = "utilities" },
	{ "<leader>w", group = "layout (windows)" },
}

wk.add(mappings)
