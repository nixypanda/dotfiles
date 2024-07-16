require("lz.n").load({
	"neotest",
	keys = {
		{ "<leader>ta", function() require("neotest").run.run({ suite = true }) end, desc = "Run [a]ll tests" },
		{
			"<leader>td",
			function() require("neotest").run.run({ strategy = "dap" }) end,
			desc = "[D]ebug nearest test",
		},
		{ "<leader>tf", function() require("neotest").run.run(vim.fn.expand("%")) end, desc = "Run [f]ile" },
		{
			"<leader>tj",
			function() require("neotest").jump.prev({ status = "failed" }) end,
			desc = "Previous failed test",
		},
		{ "<leader>tk", function() require("neotest").jump.next({ status = "failed" }) end, desc = "Next failed test" },
		{
			"<leader>to",
			function() require("neotest").output.open({ enter = true }) end,
			desc = "[O]utput of nearest test",
		},
		{ "<leader>tp", function() require("neotest").output_panel.toggle() end, desc = "Toggle raw output [p]anel" },
		{ "<leader>tr", function() require("neotest").run.run() end, desc = "[R]un nearest test" },
		{ "<leader>tt", function() require("neotest").summary.toggle() end, desc = "[T]oggle summary" },
	},
	after = function()
		require("neotest").setup({
			adapters = {
				require("neotest-go"),
				require("neotest-python")({
					dap = { justMyCode = true },
				}),
				require("rustaceanvim.neotest"),
			},
		})
	end,
})
