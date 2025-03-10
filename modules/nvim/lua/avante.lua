require("lz.n").load({
	"avante.nvim",
	keys = {
		{ "<leader>aa", "<cmd>AvanteAsk<cr>", desc = "Ask AI" },
	},
	after = function()
		require("avante").setup({
			provider = "openai",
			openai = {
				endpoint = "https://api.openai.com/v1",
				model = "gpt-3.5-turbo", -- your desired model (or use gpt-4o, etc.)
				timeout = 30000, -- timeout in milliseconds
				temperature = 0, -- adjust if needed
				max_tokens = 4096,
				-- reasoning_effort = "high" -- only supported for reasoning models (o1, etc.)
			},
		})
	end,
})
