-- Enable (broadcasting) snippet capability for completion
-- local capabilities = vim.lsp.protocol.make_client_capabilities()
-- capabilities.textDocument.completion.completionItem.snippetSupport = true
--
--
local capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol
                                                                     .make_client_capabilities())

-- bash
require'lspconfig'.bashls.setup {cmd = lang_servers_cmd.bashls}

-- cmake
-- require'lspconfig'.cmake.setup {cmd = lang_servers_cmd.cmake}

-- css
require'lspconfig'.cssls.setup {
    capabilities = capabilities,
    cmd = lang_servers_cmd.cssls
}

-- docker
require'lspconfig'.dockerls.setup {cmd = lang_servers_cmd.dockerls}

-- elm
require'lspconfig'.elmls.setup {
    cmd = lang_servers_cmd.elmls,
    init_options = {
        elmPath = lang_servers_cmd.elm,
        elmTestPath = lang_servers_cmd.elm_test,
        elmFormatPath = lang_servers_cmd.elm_format
    }
}

-- go
require'lspconfig'.gopls.setup {cmd = lang_servers_cmd.gopls}

-- Haskell
require'lspconfig'.hls.setup {
    settings = {languageServerHaskell = {formattingProvider = "brittany"}},
    cmd = lang_servers_cmd.hls
}

-- html
require'lspconfig'.html.setup {capabilities = capabilities, cmd = lang_servers_cmd.html}

-- json
require'lspconfig'.jsonls.setup {cmd = lang_servers_cmd.jsonls}

-- lua
require'lspconfig'.sumneko_lua.setup {
    cmd = {"lua-language-server"},
    settings = {Lua = {diagnostics = {globals = {'vim'}}}}
}

-- nix
require'lspconfig'.rnix.setup {cmd = lang_servers_cmd.rnix}

-- Python
require'lspconfig'.pyright.setup {cmd = lang_servers_cmd.pyright}

-- Rust
require'rust-tools'.setup()
require'lspconfig'.rust_analyzer.setup {
    on_attach = function()
        require'lsp_signature'.on_attach({
            bind = true,
            handler_opts = {border = 'single'}
        })
    end
}

-- TypeScript/JavaScript
require'lspconfig'.tsserver.setup {cmd = lang_servers_cmd.tsserver}

-- vim
require'lspconfig'.vimls.setup {cmd = lang_servers_cmd.vimls}

-- signature help
require'lsp_signature'.on_attach({bind = true, handler_opts = {border = 'single'}})

local saga = require 'lspsaga'
saga.init_lsp_saga {}
require'lspsaga.diagnostic'.show_line_diagnostics()

-- Jump to Definition/Refrences/Implementation
vim.api.nvim_set_keymap('n', 'gd', [[<cmd>lua vim.lsp.buf.definition()<CR>]],
                        {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', 'gi', [[<cmd>lua vim.lsp.buf.implementation()<CR>]],
                        {noremap = true, silent = true})
-- scroll down hover doc or scroll in definition preview
vim.api.nvim_set_keymap('n', '<C-f>',
                        [[<cmd>lua require('lspsaga.action').smart_scroll_with_saga(1)<CR>]],
                        {noremap = true, silent = true})
-- scroll up hover doc
vim.api.nvim_set_keymap('n', '<C-d>',
                        [[<cmd>lua require('lspsaga.action').smart_scroll_with_saga(-1)<CR>]],
                        {noremap = true, silent = true})

-- EFM (Various Commands as LSP) Setup
require"lspconfig".efm.setup {
    cmd = lang_servers_cmd.efmls,
    init_options = {documentFormatting = true},
    filetypes = {"css", "html", "json", "lua", "python", "markdown"},
    settings = {
        rootMarkers = {".git/"},
        languages = {
            css = {{formatCommand = lang_servers_cmd.prettier .. " --parser css"}},
            scss = {{formatCommand = lang_servers_cmd.prettier .. " --parser scss"}},
            json = {{formatCommand = lang_servers_cmd.prettier .. " --parser json"}},
            html = {{formatCommand = lang_servers_cmd.prettier .. " --parser html"}},
            markdown = {{formatCommand = 'pandoc -f markdown -t gfm -sp --tab-stop=2'}},
            python = {
                {
                    formatCommand = lang_servers_cmd.isort .. " --quiet -",
                    formatStdin = true
                },
                {
                    formatCommand = lang_servers_cmd.black .. " --quiet -",
                    formatStdin = true
                }
            },
            lua = {
                {
                    formatCommand = lang_servers_cmd.lua_format
                        .. " -i --no-keep-simple-function-one-line --no-break-after-operator --column-limit=88 --break-after-table-lb",
                    formatStdin = true
                }
            }
        }
    }
}

-- AUTO FORMATTING

-- lsp shit that can't be done in lua atm.
-- Note: This is even worse then writing vimscript (Can't believe that would be possible but here you go)
vim.api.nvim_command(
    'autocmd BufWritePre *.rs  lua vim.lsp.buf.formatting_sync(nil, 1000)')
vim.api.nvim_command(
    'autocmd BufWritePre *.hs  lua vim.lsp.buf.formatting_sync(nil, 1000)')
vim.api.nvim_command(
    'autocmd BufWritePre *.py  lua vim.lsp.buf.formatting_sync(nil, 1000)')
vim.api.nvim_command(
    'autocmd BufWritePre *.lua lua vim.lsp.buf.formatting_sync(nil, 1000)')
vim.api.nvim_command(
    'autocmd BufWritePre *.nix lua vim.lsp.buf.formatting_sync(nil, 1000)')

-- Prettify LSP diagnostic messages/icons

vim.diagnostic.config({
    virtual_text = false,
    signs = true,
    underline = true,
    update_in_insert = false,
    severity_sort = false
})

require('lspkind').init({})

vim.fn.sign_define("LspDiagnosticsSignError", {
    texthl = "LspDiagnosticsSignError",
    text = "",
    numhl = "LspDiagnosticsSignError"
})
vim.fn.sign_define("LspDiagnosticsSignWarning", {
    texthl = "LspDiagnosticsSignWarning",
    text = "",
    numhl = "LspDiagnosticsSignWarning"
})
vim.fn.sign_define("LspDiagnosticsSignHint", {
    texthl = "LspDiagnosticsSignHint",
    text = "",
    numhl = "LspDiagnosticsSignHint"
})
vim.fn.sign_define("LspDiagnosticsSignInformation", {
    texthl = "LspDiagnosticsSignInformation",
    text = "",
    numhl = "LspDiagnosticsSignInformation"
})

