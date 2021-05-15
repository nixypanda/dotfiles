require"which-key".setup {
    plugins = {
        marks = true,
        registers = true, -- shows your registers on " in NORMAL or <C-r> in INSERT mode
        -- the presets plugin, adds help for a bunch of default keybindings in Neovim
        -- No actual key bindings are created
        presets = {
            operators = true, -- operators like d, y, ...
            motions = true,
            text_objects = true, -- triggered after entering an operator
            windows = true, -- default bindings on <c-w>
            nav = true, -- misc bindings to work with windows
            z = true, -- bindings for folds, spelling and others prefixed with z
            g = true -- bindings for prefixed with g
        }
    },
    icons = {
        breadcrumb = "»", -- symbol used in the command line area that shows your active key combo
        separator = "➜", -- symbol used between a key and it's label
        group = "+" -- symbol prepended to a group
    },
    window = {
        border = "single", -- none, single, double, shadow
        position = "bottom", -- bottom, top
        margin = {1, 0, 1, 0}, -- extra window margin [top, right, bottom, left]
        padding = {1, 1, 1, 1} -- extra window padding [top, right, bottom, left]
    },
    layout = {
        height = {min = 1, max = 25}, -- min and max height of the columns
        width = {min = 20, max = 50}, -- min and max width of the columns
        spacing = 3 -- spacing between columns
    },
    hidden = {"<silent>", "<cmd>", "<Cmd>", "<CR>", "call", "lua", "^:", "^ "}, -- hide mapping boilerplate
    show_help = true -- show help message on the command line when the popup is visible
}


local mappings = {
    e = {"<cmd>NvimTreeToggle<cr>", "Explorer"},
    a = {
        name = "+Actions",
        s = {'<cmd>let @/ = ""<cr>', "Remove search highlight"},
        t = {'<cmd>TableModeToggle<cr>', "Start/Stop Table mode"}
    },

    b = {
      name = '+buffers',
      A = {'<cmd>bufdo bd<cr>'            , 'Close all buffer'},
      c = {'<cmd>BufferClose<cr>'         , 'Close this buffer'},
      C = {'<cmd>w | %bd | e#<cr>'        , 'Close all other buffers'},
      d = {'<cmd>bd<cr>'                  , 'Close buffer'},
      j = {'<cmd>BufferNext<cr>'          , 'Next Buffer'},
      k = {'<cmd>BufferPrevious<cr>'      , 'Previous Buffer'},
      J = {'<cmd>BufferMoveNext<cr>'      , 'Swap with Next Buffer'},
      K = {'<cmd>BufferMovePrevious<cr>'  , 'Swap with Previous Buffer'},
      G = {'<cmd>BufferLast<cr>'          , 'Last Buffer'},
     },

    g = {
        name = "+Git",
        b = {"<cmd>Git blame<cr>", "Blame"},
        j = {'<cmd>lua require"gitsigns".next_hunk()<CR>',       "Next Hunk"},
        k = {'<cmd>lua require"gitsigns".prev_hunk()<CR>',       "Prev Hunk"},
        p = {'<cmd>lua require"gitsigns".preview_hunk()<CR>',    "Preview Hunk"},
        s = {'<cmd>lua require"gitsigns".stage_hunk()<CR>',      "Stage Hunk"},
        u = {'<cmd>lua require"gitsigns".undo_stage_hunk()<CR>', "Undo Stage Hunk"},
        c = {
            name = "+Conflict Resolution",
            s = {"<cmd>Gdiffsplit!<cr>", "Start"},
            h = {"<cmd>diffget //2<cr>", "Get hunk from left"},
            l = {"<cmd>diffget //3<cr>", "Get hunk from right"},
            f = {"<cmd>Gwrite!<cr>", "Finish"},
        },
    },
    o = {
        name = "+Github/Octo",
        c = {
            name = "+Comment",
            a = {"<cmd>require\"octo.commands\".add_comment()<cr>",    "Add a comment"},
            d = {"<cmd>require\"octo.commands\".delete_comment()<cr>", "Delete a comment"},
            j = {"<cmd>require\"octo.navigation\".next_comment()<cr>", "Goto next comment"},
            k = {"<cmd>require\"octo.navigation\".prev_comment()<cr>", "Goto prev comment"},
        },
        p = {
            name = "+PR",
            b = {"<cmd>Octo pr browser<cr>", "Open PR in browser"},
            c = {"<cmd>Octo pr commits<cr>", "List commits"},
            f = {"<cmd>Octo pr files<cr>",   "List changed files"},
            l = {"<cmd>Octo pr list<cr>",    "List all PRs in the repo"},
        },
        r = {
            name = "+Review",
            d = {"<cmd>Octo review discard<cr>", "Discard review"},
            s = {"<cmd>Octo review start<cr>",   "Start a review"},
            S = {"<cmd>Octo review submit<cr>",  "Submit review"},
            r = {"<cmd>Octo review resume<cr>",  "Resume review"},
        },
        R = {
            name = "+Reaction",
            u = {"<cmd>Octo reaction +1<cr>", "+1"},
            d = {"<cmd>Octo reaction -1<cr>", "-1"},
        },
    },
    l = {
        name = "+LSP",
        a = {"<cmd>Lspsaga code_action<cr>",                 "Code Action"},
        d = {"<cmd>Telescope lsp_document_diagnostics<cr>",  "Document Diagnostics"},
        D = {"<cmd>Telescope lsp_workspace_diagnostics<cr>", "Workspace Diagnostics"},
        I = {"<cmd>LspInfo<cr>",                             "Info"},
        j = {"<cmd>Lspsaga diagnostic_jump_next<cr>",        "Next Action"},
        k = {"<cmd>Lspsaga diagnostic_jump_prev<cr>",        "Previous Action"},
        r = {"<cmd>Lspsaga rename<cr>",                      "Rename"},
        R = {"<cmd>Lspsaga lsp_finder<cr>",                  "Definition and Refrences"},
        l = {"<cmd>Lspsaga show_line_diagnostics<cr>",       "Line Diagnostics"},
        p = {"<cmd>Lspsaga preview_definition<cr>",          "Preview Definition"},
        t = {"<cmd>Lspsaga signature_help<cr>",              "Signature Help"},
        f = {
            name = "+Rust",
            r = {"<cmd>RustRunnables<cr>",    "Run"},
            p = {"<cmd>RustParentModule<cr>", "Goto Parent Module"},
        }
    },

    s = {
        name = "+Search",
        b = {"<cmd>Telescope buffers<cr>",               "Open Buffers"},
        f = {"<cmd>Telescope find_files<cr>",            "Find File"},
        h = {"<cmd>Telescope help_tags<cr>",             "Help Tags"},
        m = {"<cmd>Telescope marks<cr>",                 "Marks"},
        M = {"<cmd>Telescope man_pages<cr>",             "Man Pages"},
        r = {"<cmd>Telescope oldfiles<cr>",              "Open Recent File"},
        R = {"<cmd>Telescope registers<cr>",             "Registers"},
        s = {"<cmd>Telescope lsp_document_symbols<cr>",  "Document Symbols"},
        S = {"<cmd>Telescope lsp_workspace_symbols<cr>", "Workspace Symbols"},
        t = {"<cmd>Telescope live_grep<cr>",             "Text"},
        T = {"<cmd>TodoTelescope<cr>",                   "Todos"},
    },
    w = {
      name = "+Window",
      h = {"<C-w><C-h>", "Move to left window"},
      j = {"<C-w><C-j>", "Move to below window"},
      k = {"<C-w><C-k>", "Move to above window"},
      l = {"<C-w><C-l>", "Move to right window"},
      w = {"<C-w>w", "Move to other window"},
      x = {"<C-w>x", "Swap with other window"},
      s = {"<C-w>s", "Split window"},
      v = {"<C-w>v", "Split window vertically"},
      c = {"<C-w>q", "Close window"},
      o = {"<C-w>o", "Keep only current window"},
      t = {"<C-w>T", "Move window to a tab"},
      m = {
        name = "+Max",
        W = {"<C-w>|", "Max out width"},
        H = {"<C-w>_", "Max out hight"}
      },
      r = {
        name = "+Resize",
        l = {"<cmd>vertical resize +10<cr>", "Increase width"},
        k = {"<cmd>resize +10<cr>", "Increase height"},
        h = {"<cmd>vertical resize -10<cr>", "Decrease width"},
        j = {"<cmd>resize -10<cr>", "Decrease height"}
      },
      n = {"<C-w>=", "Normalize Windows"}
    }
}

local opts = {
    mode = "n", -- NORMAL mode
    prefix = "<leader>",
    buffer = nil, -- Global mappings. Specify a buffer number for buffer local mappings
    silent = true, -- use `silent` when creating keymaps
    noremap = true, -- use `noremap` when creating keymaps
    nowait = false -- use `nowait` when creating keymaps
}

local wk = require("which-key")
wk.register(mappings, opts)
