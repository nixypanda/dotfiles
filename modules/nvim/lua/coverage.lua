require("lz.n").load({
	"nvim-coverage",
	keys = {
		{ "<leader>Ct", "<cmd>CoverageToggle<cr>", desc = "Toggle coverage display" },
		{ "<leader>Cc", "<cmd>Coverage<cr>", desc = "Load coverage file and display" },
		{ "<leader>Cl", "<cmd>CoverageLoadLcov ./coverage.lcov<cr>", desc = "Load ./coverage.lcov file" },
		{ "<leader>Cs", "<cmd>CoverageSummary<cr>", desc = "Show summary of coverage" },
	},
	after = function() require("coverage").setup({}) end,
})
