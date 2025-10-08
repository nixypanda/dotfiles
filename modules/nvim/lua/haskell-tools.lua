vim.g.haskell_tools = {
	hls = {
		on_attach = function(client, bufnr)
			require("common").lsp_on_attach(client, bufnr)

			local map = function(keys, func, desc) vim.keymap.set("n", keys, func, { buffer = bufnr, desc = desc }) end
			local ht = require("haskell-tools")

			map("<leader>ps", ht.hoogle.hoogle_signature, "Hoogle search type signature")
			map("<leader>pe", ht.lsp.buf_eval_all, "Evaluate all code snippets")
			map("<leader>pr", ht.repl.toggle, "Toggle GHCi repl (package)")
			map("<leader>pR", function() ht.repl.toggle(vim.api.nvim_buf_get_name(0)) end, "Toggle GHCi repl (buffer)")
		end,
		debug = true,
		cmd = {
			"haskell-language-server-wrapper",
			"--lsp",
			"--logfile",
			vim.fn.stdpath("log") .. "/" .. "haskell-language-server.log",
		},
	},
}
