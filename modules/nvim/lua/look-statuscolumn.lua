vim.o.foldcolumn = '1' -- '0' is not bad
vim.o.foldlevel = 99   -- Using ufo provider need a large value, feel free to decrease the value
vim.o.foldlevelstart = 99
vim.o.foldenable = true
vim.o.fillchars = [[eob: ,fold: ,foldopen:,foldsep: ,foldclose:]]

vim.keymap.set('n', 'zR', require('ufo').openAllFolds)
vim.keymap.set('n', 'zM', require('ufo').closeAllFolds)

require('ufo').setup({
    provider_selector = function(bufnr, filetype, buftype)
        return { 'treesitter', 'indent' }
    end
})

local show_line_diagnostics = function(arg)
    require('lspsaga.showdiag'):show_diagnostics({ line = true, arg = arg })
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
