require("neotest").setup({
	adapters = {
		require("neotest-go"),
		require("neotest-python")({
			dap = { justMyCode = true },
		}),
		require("rustaceanvim.neotest"),
	},
})
