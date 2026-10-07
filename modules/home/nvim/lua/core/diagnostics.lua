vim.diagnostic.config({
	signs = {
		text = {
			[vim.diagnostic.severity.ERROR] = " ",
			[vim.diagnostic.severity.WARN] = " ",
			[vim.diagnostic.severity.INFO] = " ",
			[vim.diagnostic.severity.HINT] = " ",
		},
	},
	update_in_insert = false,
	severity_sort = false,
	float = { source = "if_many", border = "rounded" },
	jump = { float = true },
})
