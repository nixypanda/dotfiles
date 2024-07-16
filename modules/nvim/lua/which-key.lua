require("which-key").setup({
	layout = {
		height = { min = 1, max = 25 }, -- min and max height of the columns
	},
	icons = {
		breadcrumb = "»", -- symbol used in the command line area that shows your active key combo
		separator = "-", -- symbol used between a key and it's label
		group = "+",
	},
})

local mappings = {
	b = {
		name = "+Buffers",
		A = { "<cmd>bufdo bd<cr>", "Close all buffer" },
		c = { "<cmd>BufferClose<cr>", "Close this buffer" },
		C = { "<cmd>w | %bd | e#<cr>", "Close all other buffers" },
		d = { "<cmd>bd<cr>", "Close buffer" },
		j = { "<cmd>bnext<cr>", "Next Buffer" },
		k = { "<cmd>bperv<cr>", "Previous Buffer" },
		s = { "<cmd>tab split<cr>", "Open buffer in new tab" },
		S = { "<cmd>tab close<cr>", "Close tab" },
	},
	d = {
		name = "+Debug",
	},
	D = {
		name = "+Database",
		a = { "<cmd>DBUIAddConnection<cr>", "Add new connection" },
		t = { "<cmd>DBUIToggle<cr>", "Toggle DBUI" },
	},
	e = {
		name = "+Explorer",
	},
	g = {
		name = "+Git",
		c = {
			name = "+Conflict Resolution",
			b = { "<cmd>GitConflictChooseBoth<cr>", "Choose both" },
			h = { "<cmd>GitConflictChooseOurs<cr>", "Choose ours" },
			l = { "<cmd>GitConflictChooseTheirs<cr>", "Choose theirs" },
			n = { "<cmd>GitConflictConflictNone<cr>", "Choose none" },
			j = { "<cmd>GitConflictNextConflict<cr>", "Next conflict" },
			k = { "<cmd>GitConflictPrevConflict<cr>", "Previous conflict" },
		},
		j = { require("gitsigns").next_hunk, "Next Hunk" },
		k = { require("gitsigns").prev_hunk, "Prev Hunk" },
		p = { require("gitsigns").preview_hunk, "Preview Hunk" },
		r = { require("gitsigns").reset_hunk, "Reset Hunk" },
		s = { require("gitsigns").stage_hunk, "Stage Hunk" },
		u = { require("gitsigns").undo_stage_hunk, "Undo Stage Hunk" },
	},
	l = {
		name = "+LSP",
	},
	p = {
		name = "+Programming Language Specific",
		g = {
			name = "+Go",
			d = { function() require("dap-go").debug_test() end, "Debug nearest test" },
		},
		h = { name = "+Haskell" },
		r = { name = "+Rust" },
	},
	s = {
		name = "+Search",
		n = { "<cmd>Noice telescope<cr>", "Noice Message History" },
		T = { "<cmd>TodoTelescope<cr>", "Todos" },
	},
	t = {
		name = "+Tests",
	},
	u = {
		name = "+Utilities",
		s = { '<cmd>let @/ = ""<cr>', "Remove search highlight" },
		t = { "<cmd>TableModeToggle<cr>", "Start/Stop Table mode" },
	},
	w = {
		name = "+Window",
		c = { "<C-w>q", "Close window" },
		h = { "<C-w><C-h>", "Move to left window" },
		j = { "<C-w><C-j>", "Move to below window" },
		k = { "<C-w><C-k>", "Move to above window" },
		l = { "<C-w><C-l>", "Move to right window" },
		m = {
			name = "+Max",
			H = { "<C-w>_", "Max out hight" },
			W = { "<C-w>|", "Max out width" },
		},
		n = { "<C-w>=", "Normalize Windows" },
		o = { "<C-w>o", "Keep only current window" },
		r = {
			name = "+Resize",
			h = { "<cmd>vertical resize -10<cr>", "Decrease width" },
			j = { "<cmd>resize -10<cr>", "Decrease height" },
			k = { "<cmd>resize +10<cr>", "Increase height" },
			l = { "<cmd>vertical resize +10<cr>", "Increase width" },
		},
		s = { "<C-w>s", "Split window" },
		v = { "<C-w>v", "Split window vertically" },
		w = { "<C-w>w", "Move to other window" },
		x = { "<C-w>x", "Swap with other window" },
	},
}

local opts = {
	mode = "n", -- NORMAL mode
	prefix = "<leader>",
	buffer = nil, -- Global mappings. Specify buffer number for buffer local mappings
	silent = true, -- use `silent` when creating keymaps
	noremap = true, -- use `noremap` when creating keymaps
	nowait = false, -- use `nowait` when creating keymaps
}

local wk = require("which-key")
wk.register(mappings, opts)
