local show_line_diagnostics = function(arg)
	local ok, diagnostic_show = pcall(require, "lspsaga.diagnostic.show")
	if ok then
		diagnostic_show:show_diagnostics({ line = true, arg = arg })
	else
		vim.diagnostic.open_float(nil, { scope = "line" })
	end
end

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
