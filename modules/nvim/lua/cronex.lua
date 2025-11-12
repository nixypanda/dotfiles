require("lz.n").load({
	"cronex.nvim",
	keys = {
		{ "<leader>uc", "<cmd>CronExplainedEnable<cr>", desc = "Explain cron expressions" },
		{ "<leader>uC", "<cmd>CronExplainedDisable<cr>", desc = "Disable cron expressions explanations" },
	},
	after = function()
		require("cronex").setup({
			explainer = {
				cmd = require("nix_injected").cronex_explainer,
			},
		})
	end,
})
