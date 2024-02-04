local cmp = require 'cmp'
local lspkind = require('lspkind')

lspkind.init({})

local check_backspace = function()
    local col = vim.fn.col "." - 1
    return col == 0 or vim.fn.getline("."):sub(col, col):match "%s"
end

local feedkey = function(key, mode)
    vim.api.nvim_feedkeys(
        vim.api.nvim_replace_termcodes(key, true, true, true),
        mode,
        true
    )
end

cmp.setup({
    snippet = {
        expand = function(args)
            vim.fn["vsnip#anonymous"](args.body)
        end
    },
    formatting = { format = lspkind.cmp_format({ with_text = true, maxwidth = 50 }) },
    mapping = {
        ['<C-d>'] = cmp.mapping(cmp.mapping.scroll_docs(-4), { 'i', 'c' }),
        ['<C-f>'] = cmp.mapping(cmp.mapping.scroll_docs(4), { 'i', 'c' }),
        ['<C-Space>'] = cmp.mapping(cmp.mapping.complete(), { 'i', 'c' }),
        -- If you want to remove the default `<C-y>` mapping, You can specify
        -- `cmp.config.disable` value.
        ['<C-y>'] = cmp.config.disable,
        ['<C-e>'] = cmp.mapping({ i = cmp.mapping.abort(), c = cmp.mapping.close() }),
        ['<CR>'] = cmp.mapping.confirm({ select = false }),
        ["<Tab>"] = cmp.mapping(function(fallback)
            if cmp.visible() then
                cmp.select_next_item()
            elseif vim.fn["vsnip#expandable"]() == 1 then
                feedkey("<Plug>(vsnip-expand)", "")
            elseif vim.fn["vsnip#available"](1) == 1 then
                feedkey("<Plug>(vsnip-expand-or-jump)", "")
            elseif check_backspace() then
                fallback()
            else
                fallback()
            end
        end, { "i", "s" }),
        ["<S-Tab>"] = cmp.mapping(function(fallback)
            if cmp.visible() then
                cmp.select_prev_item()
            elseif vim.fn["vsnip#jumpable"](-1) == 1 then
                feedkey("<Plug>(vsnip-jump-prev)", "")
            else
                fallback()
            end
        end, { "i", "s" })
    },
    sources = cmp.config.sources(
        {
            { name = 'nvim_lsp', max_item_count = 10 },
            { name = 'vsnip' },
            { name = 'nvim_lua' },
            { name = 'calc' },
            { name = 'crates' }
        },
        {
            { name = 'buffer', max_item_count = 5, keyword_length = 3 },
            { name = 'path' }
        }
    ),
    window = {
        completion = cmp.config.window.bordered(),
        documentation = cmp.config.window.bordered(),
    },
})

cmp.setup.cmdline(':', {
    mapping = cmp.mapping.preset.cmdline(),
    sources = cmp.config.sources({
        { name = 'path' }
    }, {
        {
            name = 'cmdline',
            option = {
                ignore_cmds = { 'Man', '!' }
            }
        }
    })
})

-- Set loction for snippets to be in dotfiles repo so that it is accessible everywhere
vim.g.vsnip_snippet_dir = vim.fn.expand('~/.dotfiles/users/modules/nvim/vsnip')

-- Autopairs
require('nvim-autopairs').setup {}

-- Completion (default setting)
vim.o.completeopt = "menuone,noselect"
