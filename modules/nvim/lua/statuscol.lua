local show_line_diagnostics = function(arg)
    require('lspsaga.diagnostic.show'):show_diagnostics({ line = true, arg = arg })
end


local builtin = require('statuscol.builtin')
require('statuscol').setup({
    relculright = true,
    segments = {
        { text = { "%s" },                  click = "v:lua.ScSa" },
        { text = { builtin.foldfunc },      click = "v:lua.ScFa" },
        { text = { builtin.lnumfunc, " " }, click = "v:lua.ScLa" },
    },
    clickhandlers = {
        DiagnosticSignError = show_line_diagnostics,
        DiagnosticSignHint  = show_line_diagnostics,
        DiagnosticSignInfo  = show_line_diagnostics,
        DiagnosticSignWarn  = show_line_diagnostics,
    }
})
