require("lz.n").load({
	"haskell-tools.nvim",
	keys = {

		{
			"<leader>phe",
			function() require("haskell-tools").lsp.buf_eval_all() end,
			desc = "[E]val code snippets in buffer",
		},
		{ "<leader>phl", vim.lsp.codelens.run, desc = "Code[L]ens" },
		{ "<leader>phr", function() require("haskell-tools").repl.toggle() end, desc = "[R]epl for current package" },
		{
			"<leader>phR",
			function() require("haskell-tools").repl.toggle(vim.api.nvim_buf_get_name(0)) end,
			desc = "[R]epl for current buffer",
		},
	},
	before = function()
		local haskell_local_config = {
			hls_logfile_location = vim.fn.stdpath("log") .. "/" .. "haskell-language-server.log",
		}

		vim.g.haskell_tools = {
			tools = {},
			hls = {
				debug = true,
				cmd = {
					"haskell-language-server-wrapper",
					"--lsp",
					"--log-level",
					"Warning",
					"--logfile",
					haskell_local_config.hls_logfile_location,
				},
			},
			dap = {},
		}
	end,
})
