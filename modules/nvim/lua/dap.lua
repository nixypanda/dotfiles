require("lz.n").load({
	"nvim-dap",
	keys = { "<leader>db", "<leader>ds" },
	load = function(name)
		vim.cmd.packadd(name)
		vim.cmd.packadd("nvim-dap-ui")
		vim.cmd.packadd("nvim-dap-virtual-text")
		vim.cmd.packadd("nvim-dap-python")
		vim.cmd.packadd("nvim-dap-go")
	end,
	after = function()
		-- General Setup
		local dap = require("dap")
		local dapui = require("dapui")
		local widgets = require("dap.ui.widgets")

		-- Setup keymaps
		vim.keymap.set("n", "<leader>db", dap.toggle_breakpoint, { desc = "Toggle [b]reakpoint" })
		vim.keymap.set("n", "<leader>df", dap.close, { desc = "[F]inish debugging" })
		vim.keymap.set("n", "<leader>di", widgets.hover, { desc = "[I]nspect variable under cursor" })
		vim.keymap.set("n", "<leader>dI", dap.step_into, { desc = "Step [i]nto" })
		vim.keymap.set("n", "<leader>dj", dap.down, { desc = "Go [down] in call stack" })
		vim.keymap.set("n", "<leader>dk", dap.up, { desc = "Go [up] in call stack" })
		vim.keymap.set("n", "<leader>do", dap.step_over, { desc = "Step [o]ver" })
		vim.keymap.set("n", "<leader>dO", dap.step_out, { desc = "Step [o]ut" })
		vim.keymap.set("n", "<leader>ds", dap.continue, { desc = "[S]tart debugging" })
		vim.keymap.set("n", "<leader>dt", dap.terminate, { desc = "[T]erminate debugging" })
		vim.keymap.set(
			"n",
			"<leader>dS",
			function() widgets.centered_float(widgets.scopes) end,
			{ desc = "Show [S]copes" }
		)

		dapui.setup()
		dap.listeners.before.attach.dapui_config = function() dapui.open() end
		dap.listeners.before.launch.dapui_config = function() dapui.open() end
		dap.listeners.before.event_terminated.dapui_config = function() dapui.close() end
		dap.listeners.before.event_exited.dapui_config = function() dapui.close() end

		require("nvim-dap-virtual-text").setup()
		require("dap-go").setup()

		local dap_python = require("dap-python")
		-- Injected via nix
		dap_python.setup(python_with_debugpy .. "/bin/python")
		dap_python.test_runner = "pytest"

		-- TODO: Find a nice way to do this, so it picks up theme colors
		vim.fn.sign_define("DapBreakpoint", { text = "⊕", texthl = "Error", linehl = "", numhl = "" })
		vim.fn.sign_define("DapStopped", { text = "→", texthl = "Info", linehl = "Info", numhl = "" })
	end,
})
