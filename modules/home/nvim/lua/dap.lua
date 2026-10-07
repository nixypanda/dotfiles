require("lz.n").load({
	"nvim-dap",
	keys = {
		{ "<leader>db", function() require("dap").toggle_breakpoint() end, desc = "Toggle breakpoint" },
		{ "<leader>ds", function() require("dap").continue() end, desc = "Start debugging" },
		{ "<leader>dt", function() require("dap-view").toggle() end, desc = "Toggle debugging UI" },
	},
	load = function(name)
		vim.cmd.packadd(name)
		vim.cmd.packadd("nvim-dap-view")
		vim.cmd.packadd("nvim-dap-python")
	end,
	after = function()
		local dap = require("dap")
		local widgets = require("dap.ui.widgets")

		-- Setup keymaps
		vim.keymap.set("n", "<leader>df", dap.close, { desc = "Finish debugging" })
		vim.keymap.set("n", "<leader>di", widgets.hover, { desc = "Inspect variable under cursor" })
		vim.keymap.set("n", "<leader>dI", dap.step_into, { desc = "Step into" })
		vim.keymap.set("n", "<leader>dj", dap.down, { desc = "Go down in call stack" })
		vim.keymap.set("n", "<leader>dk", dap.up, { desc = "Go up in call stack" })
		vim.keymap.set("n", "<leader>do", dap.step_over, { desc = "Step over" })
		vim.keymap.set("n", "<leader>dO", dap.step_out, { desc = "Step out" })
		vim.keymap.set("n", "<leader>dr", function() require("dap-view").open() end, { desc = "Open debugging UI" })
		vim.keymap.set("n", "<leader>dT", dap.terminate, { desc = "Terminate debugging" })
		vim.keymap.set(
			"n",
			"<leader>dS",
			function() widgets.centered_float(widgets.scopes) end,
			{ desc = "Show Scopes" }
		)

		require("dap-view").setup({
			winbar = {
				controls = {
					enabled = true,
				},
			},
			auto_toggle = true,
			virtual_text = {
				enabled = true,
			},
		})

		-- Programming language specific plugins

		-- python
		local dap_python = require("dap-python")
		-- Injected via nix: python_with_debugpy (location of python install with debugpy)
		dap_python.setup(require("nix_injected").dap_python_with_debugpy .. "/bin/python")
		dap_python.test_runner = "pytest"

		-- UI Icons
		vim.fn.sign_define("DapBreakpoint", { text = "⊕", texthl = "Error", linehl = "", numhl = "" })
	end,
})
