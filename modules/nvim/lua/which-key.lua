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
		b = {
			function()
				require("dap").toggle_breakpoint()
			end,
			"Toggle breakpoint",
		},
		i = {
			function()
				require("dap").step_into()
			end,
			"Step into",
		},
		o = {
			function()
				require("dap").step_over()
			end,
			"Step over",
		},
		O = {
			function()
				require("dap").step_out()
			end,
			"Step out",
		},
		I = {
			function()
				require("dap.ui.widgets").hover()
			end,
			"Inspect variable under cursor",
		},
		S = {
			function()
				local w = require("dap.ui.widgets")
				w.centered_float(w.scopes)
			end,
			"Show Scopes",
		},
		s = {
			function()
				require("dap").continue()
			end,
			"Start debugging",
		},
		t = {
			function()
				require("dap").terminate()
			end,
			"Terminate debugging",
		},
		f = {
			function()
				require("dap").close()
			end,
			"Finish debugging",
		},
		j = {
			function()
				require("dap").down()
			end,
			"Go down in call stack",
		},
		k = {
			function()
				require("dap").up()
			end,
			"Go up in call stack",
		},
	},
	D = {
		name = "+Database",
		a = { "<cmd>DBUIAddConnection<cr>", "Add new connection" },
		t = { "<cmd>DBUIToggle<cr>", "Toggle DBUI" },
	},
	g = {
		name = "+Git",
		b = { "<cmd>Git blame<cr>", "Blame" },
		j = {
			function()
				require("gitsigns").next_hunk()
			end,
			"Next Hunk",
		},
		k = {
			function()
				require("gitsigns").prev_hunk()
			end,
			"Prev Hunk",
		},
		p = {
			function()
				require("gitsigns").preview_hunk()
			end,
			"Preview Hunk",
		},
		r = {
			function()
				require("gitsigns").reset_hunk()
			end,
			"Reset Hunk",
		},
		s = {
			function()
				require("gitsigns").stage_hunk()
			end,
			"Stage Hunk",
		},
		u = {
			function()
				require("gitsigns").undo_stage_hunk()
			end,
			"Undo Stage Hunk",
		},
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
		D = {
			function()
				require("telescope.builtin").diagnostics()
			end,
			"Workspace Diagnostics",
		},
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
		s = {
			function()
				require("telescope.builtin").lsp_document_symbols()
			end,
			"Document Symbols",
		},
		S = {
			function()
				require("telescope.builtin").lsp_dynamic_workspace_symbols()
			end,
			"Workspace Symbols",
		},
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
			l = {
				function()
					vim.lsp.codelens.run()
				end,
				"CodeLens",
			},
			e = {
				function()
					require("haskell-tools").lsp.buf_eval_all()
				end,
				"Eval code snippets in buffer",
			},
			r = {
				function()
					require("haskell-tools").repl.toggle()
				end,
				"Repl for current package",
			},
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
		b = {
			function()
				require("telescope.builtin").buffers()
			end,
			"Open Buffers",
		},
		c = {
			function()
				require("telescope.builtin").command_history()
			end,
			"Previous commands",
		},
		C = {
			function()
				require("telescope.builtin").commands()
			end,
			"Available commands",
		},
		f = {
			function()
				require("telescope.builtin").find_files()
			end,
			"Find File",
		},
		H = {
			function()
				require("telescope.builtin").help_tags()
			end,
			"Help Tags",
		},
		j = {
			function()
				require("telescope.builtin").jumplist()
			end,
			"Jump List",
		},
		m = {
			function()
				require("telescope.builtin").marks()
			end,
			"Marks",
		},
		r = {
			function()
				require("telescope.builtin").resume()
			end,
			"Goto last search state",
		},
		R = {
			function()
				require("telescope.builtin").registers()
			end,
			"Registers",
		},
		t = {
			function()
				require("telescope.builtin").live_grep()
			end,
			"Text",
		},
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
