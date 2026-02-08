require("lz.n").load({
	"opencode.nvim",
	keys = {
		{
			"<leader>aa",
			function() require("opencode").ask("@this: ", { submit = true }) end,
			desc = "Ask opencode…",
			mode = { "n", "t", "v" },
		},
		{ "<leader>ax", function() require("opencode").select() end, desc = "Execute opencode action…" },
		{ "<leader>at", function() require("opencode").toggle() end, desc = "Toggle opencode", mode = { "n", "t" } },
	},
	after = function()
		vim.g.opencode_opts = {
			provider = {
				enabled = "kitty",
			},
		}
		vim.o.autoread = true
	end,
})
