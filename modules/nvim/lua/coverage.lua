require("lz.n").load({
	"nvim-coverage",
	keys = {
		{ "<leader>ct", "<cmd>CoverageToggle<cr>", desc = "Toggle coverage display" },
		{ "<leader>cc", "<cmd>Coverage<cr>", desc = "Load coverage file and display" },
		{ "<leader>cl", "<cmd>CoverageLoadLcov ./coverage.lcov<cr>", desc = "Load ./coverage.lcov file" },
		{ "<leader>cs", "<cmd>CoverageSummary<cr>", desc = "Show summary of coverage" },
	},
	after = function()
		require("coverage").setup({
			signs = {
				-- use your own highlight groups or text markers
				covered = { hl = "CoverageCovered", text = "" },
			},
		})
	end,
})
