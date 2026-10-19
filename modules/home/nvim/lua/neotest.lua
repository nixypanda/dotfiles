require("lz.n").load({
	"neotest",
	keys = {
		{
			"<leader>td",
			function() require("neotest").run.run({ strategy = "dap" }) end,
			desc = "Debug nearest test",
		},
		{ "<leader>tr", function() require("neotest").run.run() end, desc = "Run nearest test" },
	},
	load = function(name)
		vim.cmd.packadd(name)
		vim.cmd.packadd("neotest-python")
		vim.cmd.packadd("rustaceanvim")
		vim.cmd.packadd("neotest-haskell")
	end,
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
