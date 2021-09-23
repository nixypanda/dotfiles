-- Enable (broadcasting) snippet capability for completion
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true

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
    cmd = lang_servers_cmd.rust_analyzer,
    settings = {
        ['rust-analyzer'] = {
            checkOnSave = {
                allFeatures = true,
                overrideCommand = {
                    lang_servers_cmd.clippy, '--workspace', '--message-format=json',
                    '--all-targets', '--all-features'
                }
            }
            -- TODO: Fix this
            -- rustfmt = {overrideCommand = {lang_servers_cmd.rustfmt, "--"}}
        }
    },
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
vim.api.nvim_set_keymap('n', '<C-b>',
                        [[<cmd>lua require('lspsaga.action').smart_scroll_with_saga(-1)<CR>]],
                        {noremap = true, silent = true})

-- Autopairs
require('nvim-autopairs').setup()

-- Completion
vim.o.completeopt = "menuone,noselect"

require'compe'.setup {
    enabled = true,
    autocomplete = true,
    debug = false,
    min_length = 1,
    preselect = 'enable',
    throttle_time = 80,
    source_timeout = 200,
    incomplete_delay = 400,
    max_abbr_width = 100,
    max_kind_width = 100,
    max_menu_width = 100,
    documentation = true,

    source = {
        path = true,
        buffer = true,
        calc = true,
        vsnip = true,
        nvim_lsp = true,
        nvim_lua = true,
        spell = true,
        tags = true,
        snippets_nvim = false,
        treesitter = false
    }
}

local t = function(str)
    return vim.api.nvim_replace_termcodes(str, true, true, true)
end

local check_back_space = function()
    local col = vim.fn.col('.') - 1
    if col == 0 or vim.fn.getline('.'):sub(col, col):match('%s') then
        return true
    else
        return false
    end
end

-- Use (s-)tab to:
--- move to prev/next item in completion menuone
--- jump to prev/next snippet's placeholder
_G.tab_complete = function()
    if vim.fn.pumvisible() == 1 then
        return t "<C-n>"
    elseif vim.fn.call("vsnip#available", {1}) == 1 then
        return t "<Plug>(vsnip-expand-or-jump)"
    elseif check_back_space() then
        return t "<Tab>"
    else
        return vim.fn['compe#complete']()
    end
end
_G.s_tab_complete = function()
    if vim.fn.pumvisible() == 1 then
        return t "<C-p>"
    elseif vim.fn.call("vsnip#jumpable", {-1}) == 1 then
        return t "<Plug>(vsnip-jump-prev)"
    else
        return t "<S-Tab>"
    end
end

vim.api.nvim_set_keymap("i", "<Tab>", "v:lua.tab_complete()", {expr = true})
vim.api.nvim_set_keymap("s", "<Tab>", "v:lua.tab_complete()", {expr = true})
vim.api.nvim_set_keymap("i", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})
vim.api.nvim_set_keymap("s", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})

-- Before        Input         After
-- ------------------------------------
-- {|}           <CR>          {
--                                 |
--                             }
local npairs = require('nvim-autopairs')
vim.g.completion_confirm_key = ""
_G.MUtils = {}
MUtils.completion_confirm = function()
    if vim.fn.pumvisible() ~= 0 then
        if vim.fn.complete_info()["selected"] ~= -1 then
            vim.fn["compe#confirm"]()
            return npairs.esc("")
        else
            vim.api.nvim_select_popupmenu_item(0, false, false, {})
            vim.fn["compe#confirm"]()
            return npairs.esc("<c-n>")
        end
    else
        return npairs.check_break_line_char()
    end
end

vim.api.nvim_set_keymap('i', '<CR>', 'v:lua.MUtils.completion_confirm()',
                        {expr = true, noremap = true})

-- Prettify LSP shit

require('lspkind').init({})

vim.lsp.handlers["textDocument/publishDiagnostics"] =
    vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
        underline = true,
        -- Enable virtual text, override spacing to 4
        virtual_text = false,
        signs = true
    })

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

-- Set loction for snippets to be in dotfiles repo so that it is accessible everywhere
vim.g.vsnip_snippet_dir = vim.fn.expand('~/.dotfiles/users/modules/nvim/vsnip')

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

-- EFM Setup
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
