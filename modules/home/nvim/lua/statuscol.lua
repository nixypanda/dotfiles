local show_line_diagnostics = function() vim.diagnostic.open_float({ scope = "line" }) end

local builtin = require("statuscol.builtin")
require("statuscol").setup({
	relculright = true,
	segments = {
		{ text = { "%s" }, click = "v:lua.ScSa" },
		{ text = { builtin.foldfunc }, click = "v:lua.ScFa" },
		{ text = { builtin.lnumfunc, " " }, click = "v:lua.ScLa" },
	},
	clickhandlers = {
		["diagnostic/signs"] = show_line_diagnostics,
	},
})
