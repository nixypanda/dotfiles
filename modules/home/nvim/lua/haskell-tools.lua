vim.g.haskell_tools = {
	hls = {
		cmd = {
			"haskell-language-server-wrapper",
			"--lsp",
			"--log-level",
			"Warning",
			"--log-file",
			vim.fn.stdpath("log") .. "/haskell-language-server.log",
			"--log-stderr",
			"False",
		},
		on_attach = function(client, bufnr)
			local common = require("common")
			common.lsp_on_attach(client, bufnr)
			local map = common.buf_map(bufnr)
			local ht = require("haskell-tools")

			map("<leader>ps", ht.hoogle.hoogle_signature, "Hoogle search type signature")
			map("<leader>pe", ht.lsp.buf_eval_all, "Evaluate all code snippets")
			map("<leader>pr", ht.repl.toggle, "Toggle GHCi repl (package)")
			map("<leader>pR", function() ht.repl.toggle(vim.api.nvim_buf_get_name(0)) end, "Toggle GHCi repl (buffer)")
		end,
	},
}
