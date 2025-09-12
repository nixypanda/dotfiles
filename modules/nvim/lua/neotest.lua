require("lz.n").load({
	"neotest",
	keys = {
		{ "<leader>ta", function() require("neotest").run.run({ suite = true }) end, desc = "Run all tests" },
		{
			"<leader>td",
			function() require("neotest").run.run({ strategy = "dap" }) end,
			desc = "Debug nearest test",
		},
		{ "<leader>tf", function() require("neotest").run.run(vim.fn.expand("%")) end, desc = "Run file" },
		{
			"<leader>tj",
			function() require("neotest").jump.prev({ status = "failed" }) end,
			desc = "Previous failed test",
		},
		{ "<leader>tk", function() require("neotest").jump.next({ status = "failed" }) end, desc = "Next failed test" },
		{
			"<leader>to",
			function() require("neotest").output.open({ enter = true }) end,
			desc = "Output of nearest test",
		},
		{ "<leader>tp", function() require("neotest").output_panel.toggle() end, desc = "Toggle raw output panel" },
		{ "<leader>tr", function() require("neotest").run.run() end, desc = "Run nearest test" },
		{ "<leader>tt", function() require("neotest").summary.toggle() end, desc = "Toggle summary" },
	},
	after = function()
		require("neotest").setup({
			adapters = {
				require("neotest-python")({
					dap = { justMyCode = true },
				}),
				require("rustaceanvim.neotest"),
				require("neotest-haskell"),
			},
		})
	end,
})
