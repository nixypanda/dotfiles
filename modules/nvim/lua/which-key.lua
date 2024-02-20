require("which-key").setup({
	layout = {
		height = { min = 1, max = 25 }, -- min and max height of the columns
	},
})

local mappings = {
	e = { "<cmd>NvimTreeToggle<cr>", "Explorer" },
	a = {
		name = "+AI assistant",
		c = { "<cmd>ChatGPT<cr>", "ChatGPT" },
	},
	b = {
		name = "+Buffers",
		A = { "<cmd>bufdo bd<cr>", "Close all buffer" },
		c = { "<cmd>BufferClose<cr>", "Close this buffer" },
		C = { "<cmd>w | %bd | e#<cr>", "Close all other buffers" },
		d = { "<cmd>bd<cr>", "Close buffer" },
		j = { "<cmd>bnext<cr>", "Next Buffer" },
		k = { "<cmd>bperv<cr>", "Previous Buffer" },
	},
	c = {
		name = "+CodeEval",
		e = { "<cmd>ConjureEvalRootForm<cr>", "Eval" },
		l = { "<cmd>ConjureLogVSplit<cr>", "Show log (vertical split)" },
	},
	d = {
		name = "+Debug",
		b = { require("dap").toggle_breakpoint, "Toggle breakpoint" },
		i = { require("dap").step_into, "Step into" },
		o = { require("dap").step_over, "Step over" },
		O = { require("dap").step_out, "Step out" },
		I = { require("dap.ui.widgets").hover, "Inspect variable under cursor" },
		S = {
			function()
				local w = require("dap.ui.widgets")
				w.centered_float(w.scopes)
			end,
			"Show Scopes",
		},
		s = { require("dap").continue, "Start debugging" },
		t = { require("dap").terminate, "Terminate debugging" },
		f = { require("dap").close, "Finish debugging" },
		j = { require("dap").down, "Go down in call stack" },
		k = { require("dap").up, "Go up in call stack" },
	},
	D = {
		name = "+Database",
		a = { "<cmd>DBUIAddConnection<cr>", "Add new connection" },
		t = { "<cmd>DBUIToggle<cr>", "Toggle DBUI" },
	},
	g = {
		name = "+Git",
		b = { "<cmd>Git blame<cr>", "Blame" },
		j = { require("gitsigns").next_hunk, "Next Hunk" },
		k = { require("gitsigns").prev_hunk, "Prev Hunk" },
		p = { require("gitsigns").preview_hunk, "Preview Hunk" },
		r = { require("gitsigns").reset_hunk, "Reset Hunk" },
		s = { require("gitsigns").stage_hunk, "Stage Hunk" },
		u = { require("gitsigns").undo_stage_hunk, "Undo Stage Hunk" },
		c = {
			name = "+Conflict Resolution",
			s = { "<cmd>Gdiffsplit!<cr>", "Start" },
			-- Fugitive follows a consistent naming convention when creating
			-- buffers for the target and merge versions of a conflicted file.
			-- The parent file from the target branch always includes the
			-- string //2, while the parent from the merge branch always
			-- contains //3.
			h = { "<cmd>diffget //2<cr>", "Get hunk from left (target)" },
			l = { "<cmd>diffget //3<cr>", "Get hunk from right (merge)" },
			f = { "<cmd>Gwrite!<cr>", "Finish" },
		},
	},
	l = {
		name = "+LSP",
		a = {
			function()
				require("lspsaga.codeaction"):code_action()
			end,
			"Code Action",
		},
		c = {
			function()
				require("lspsaga.diagnostic.show"):show_diagnostics({ cursor = true, args = {} })
			end,
			"Cursor Diagnostics",
		},
		d = {
			function()
				require("telescope.builtin").diagnostics({ bufnr = 0 })
			end,
			"Document Diagnostics",
		},
		D = { require("telescope.builtin").diagnostics, "Workspace Diagnostics" },
		f = {
			function()
				require("lspsaga.finder"):new({})
			end,
			"Refrences and implementations",
		},
		i = {
			function()
				require("lspsaga.callhierarchy"):send_method(2, {})
			end,
			"Incoming Calls",
		},
		I = { "<cmd>LspInfo<cr>", "Info" },
		j = {
			function()
				require("lspsaga.diagnostic"):goto_next()
			end,
			"Next Action",
		},
		k = {
			function()
				require("lspsaga.diagnostic"):goto_prev()
			end,
			"Previous Action",
		},
		l = {
			function()
				require("lspsaga.diagnostic.show"):show_diagnostics({ line = true, args = {} })
			end,
			"Line Diagnostics",
		},
		o = {
			function()
				require("lspsaga.callhierarchy"):send_method(3, {})
			end,
			"Outgoing Calls",
		},
		O = {
			function()
				require("lspsaga.symbol"):outline()
			end,
			"Toggle Document Symbols Outline",
		},
		p = {
			function()
				require("lspsaga.hover"):render_hover_doc({})
			end,
			"Preview Definition",
		},
		r = {
			function()
				require("lspsaga.rename"):lsp_rename({})
			end,
			"Rename",
		},
		s = { require("telescope.builtin").lsp_document_symbols, "Document Symbols" },
		S = { require("telescope.builtin").lsp_dynamic_workspace_symbols, "Workspace Symbols" },
		u = { "<cmd>LspRestart<cr>", "Restart LSP" },
		U = { "<cmd>LspStart<cr>", "Start LSP" },
	},
	p = {
		name = "+Programming Language Specific",
		g = {
			name = "+Go",
			d = {
				function()
					require("dap-go").debug_test()
				end,
				"Debug nearest test",
			},
		},
		r = {
			name = "+Rust",
			d = { "<cmd>RustLsp debugables<cr>", "Debugables" },
			e = { "<cmd>RustLsp explainError<cr>", "Explain Error" },
			r = { "<cmd>RustLsp runnables<cr>", "Runnables" },
			t = { "<cmd>RustLsp testables<cr>", "Testables" },
		},
		h = {
			name = "+Haskell",
			l = { vim.lsp.codelens.run, "CodeLens" },
			e = { require("haskell-tools").lsp.buf_eval_all, "Eval code snippets in buffer" },
			r = { require("haskell-tools").repl.toggle, "Repl for current package" },
			R = {
				function()
					require("haskell-tools").repl.toggle(vim.api.nvim_buf_get_name(0))
				end,
				"Repl for current buffer",
			},
		},
	},
	s = {
		name = "+Search",
		b = { require("telescope.builtin").buffers, "Open Buffers" },
		c = { require("telescope.builtin").command_history, "Previous commands" },
		C = { require("telescope.builtin").commands, "Available commands" },
		f = { require("telescope.builtin").find_files, "Find File" },
		H = { require("telescope.builtin").help_tags, "Help Tags" },
		j = { require("telescope.builtin").jumplist, "Jump List" },
		m = { require("telescope.builtin").marks, "Marks" },
		M = { "<cmd>Noice telescope<cr>", "Message History" },
		r = { require("telescope.builtin").resume, "Goto last search state" },
		R = { require("telescope.builtin").registers, "Registers" },
		t = { require("telescope.builtin").live_grep, "Text" },
		T = { "<cmd>TodoTelescope<cr>", "Todos" },
	},
	t = {
		name = "+Tests",
		a = {
			function()
				require("neotest").run.run({ suite = true })
			end,
			"Run all tests",
		},
		d = {
			function()
				require("neotest").run.run({ strategy = "dap" })
			end,
			"Debug nearest test",
		},
		f = {
			function()
				require("neotest").run.run(vim.fn.expand("%"))
			end,
			"Run file",
		},
		j = {
			function()
				require("neotest").jump.prev({ status = "failed" })
			end,
			"Previous failed test",
		},
		k = {
			function()
				require("neotest").jump.next({ status = "failed" })
			end,
			"Next failed test",
		},
		o = {
			function()
				require("neotest").output.open({ enter = true })
			end,
			"Open output for nearest test",
		},
		p = {
			function()
				require("neotest").output_panel.toggle()
			end,
			"Toggle raw output panel",
		},
		r = {
			function()
				require("neotest").run.run()
			end,
			"Run nearest test",
		},
		t = {
			function()
				require("neotest").summary.toggle()
			end,
			"Toggle summary",
		},
	},
	u = {
		name = "+Utilities",
		s = { '<cmd>let @/ = ""<cr>', "Remove search highlight" },
		t = { "<cmd>TableModeToggle<cr>", "Start/Stop Table mode" },
		r = { "<cmd>RegexplainerToggle<cr>", "Explain Regex (Show/Hide)" },
	},
	w = {
		name = "+Window",
		h = { "<C-w><C-h>", "Move to left window" },
		j = { "<C-w><C-j>", "Move to below window" },
		k = { "<C-w><C-k>", "Move to above window" },
		l = { "<C-w><C-l>", "Move to right window" },
		w = { "<C-w>w", "Move to other window" },
		x = { "<C-w>x", "Swap with other window" },
		s = { "<C-w>s", "Split window" },
		v = { "<C-w>v", "Split window vertically" },
		c = { "<C-w>q", "Close window" },
		o = { "<C-w>o", "Keep only current window" },
		m = {
			name = "+Max",
			W = { "<C-w>|", "Max out width" },
			H = { "<C-w>_", "Max out hight" },
		},
		r = {
			name = "+Resize",
			l = { "<cmd>vertical resize +10<cr>", "Increase width" },
			k = { "<cmd>resize +10<cr>", "Increase height" },
			h = { "<cmd>vertical resize -10<cr>", "Decrease width" },
			j = { "<cmd>resize -10<cr>", "Decrease height" },
		},
		n = { "<C-w>=", "Normalize Windows" },
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
