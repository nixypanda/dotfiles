require "which-key".setup {
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
        margin = { 1, 0, 1, 0 }, -- extra window margin [top, right, bottom, left]
        padding = { 1, 1, 1, 1 } -- extra window padding [top, right, bottom, left]
    },
    layout = {
        height = { min = 1, max = 25 }, -- min and max height of the columns
        width = { min = 20, max = 50 }, -- min and max width of the columns
        spacing = 3 -- spacing between columns
    },
    hidden = { "<silent>", "<cmd>", "<Cmd>", "<CR>", "call", "lua", "^:", "^ " }, -- hide mapping boilerplate
    show_help = true -- show help message on the command line when the popup is visible
}

local mappings = {
    e = { "<cmd>NvimTreeToggle<cr>", "Explorer" },
    a = {
        name = "+Actions",
        s = { '<cmd>let @/ = ""<cr>', "Remove search highlight" },
        t = { '<cmd>TableModeToggle<cr>', "Start/Stop Table mode" },
        d = { '<cmd>lua require"better-digraphs".digraphs("i")<cr>', "Digraphs" },
        r = { '<cmd>RegexplainerToggle<cr>', "Explain Regex (Show/Hide)" },
    },
    b = {
        name = '+buffers',
        A = { '<cmd>bufdo bd<cr>', 'Close all buffer' },
        c = { '<cmd>BufferClose<cr>', 'Close this buffer' },
        C = { '<cmd>w | %bd | e#<cr>', 'Close all other buffers' },
        d = { '<cmd>bd<cr>', 'Close buffer' },
        j = { '<cmd>BufferNext<cr>', 'Next Buffer' },
        k = { '<cmd>BufferPrevious<cr>', 'Previous Buffer' },
        J = { '<cmd>BufferMoveNext<cr>', 'Swap with Next Buffer' },
        K = { '<cmd>BufferMovePrevious<cr>', 'Swap with Previous Buffer' },
        G = { '<cmd>BufferLast<cr>', 'Last Buffer' }
    },
    d = {
        name = "+dap",
        b = { "<cmd>lua require'dap'.toggle_breakpoint()<cr>", "Toggle breakpoint" },
        i = { "<cmd>lua require'dap'.step_into()<cr>", "Step into" },
        o = { "<cmd>lua require'dap'.step_over()<cr>", "Step over" },
        O = { "<cmd>lua require'dap'.step_out()<cr>", "Step out" },
        I = {
            "<cmd>lua require'dap.ui.widgets'.hover()<cr>",
            "Inspect variable under cursor"
        },
        S = {
            "<cmd>lua local w=require'dap.ui.widgets';w.centered_float(w.scopes)<cr>",
            "Show Scopes"
        },
        s = { "<cmd>lua require'dap'.continue()<cr>", "Start debugging" },
        t = { "<cmd>lua require'dap'.terminate()<cr>", "Terminate debugging" },
        f = { "<cmd>lua require'dap'.close()<cr>", "Finish debugging" },
        j = { "<cmd>lua require'dap'.down()<cr>", "Go down in call stack" },
        k = { "<cmd>lua require'dap'.up()<cr>", "Go up in call stack" }
    },
    g = {
        name = "+Git",
        b = { "<cmd>Git blame<cr>", "Blame" },
        j = { '<cmd>lua require"gitsigns".next_hunk()<CR>', "Next Hunk" },
        k = { '<cmd>lua require"gitsigns".prev_hunk()<CR>', "Prev Hunk" },
        p = { '<cmd>lua require"gitsigns".preview_hunk()<CR>', "Preview Hunk" },
        r = { '<cmd>Gitsigns reset_hunk<CR>', "Reset Hunk" },
        s = { '<cmd>lua require"gitsigns".stage_hunk()<CR>', "Stage Hunk" },
        u = { '<cmd>lua require"gitsigns".undo_stage_hunk()<CR>', "Undo Stage Hunk" },
        c = {
            name = "+Conflict Resolution",
            s = { "<cmd>Gdiffsplit!<cr>", "Start" },
            -- Fugitive follows a consistent naming convention when creating
            -- buffers for the target and merge versions of a conflicted file.
            -- The parent file from the target branch always includes the
            -- string //2, while the parent from the merge branch always
            -- contains //3.
            h = { "<cmd>diffget //2<cr>", "Get hunk from left (target)" },
            l = { "<cmd>diffget //3<cr>", "Get hunk from right (merge)" },
            f = { "<cmd>Gwrite!<cr>", "Finish" }
        }
    },
    l = {
        name = "+LSP",
        a = { "<cmd>Lspsaga code_action<cr>", "Code Action" },
        c = {
            name = "+Call heriarchy",
            i = { "<cmd>Lspsaga incoming_calls<cr>", "Hover Doc" },
            o = { "<cmd>Lspsaga outgoing_calls<cr>", "Signature Help" },
        },
        d = { "<cmd>Telescope diagnostics bufnr=0<cr>", "Document Diagnostics" },
        D = { "<cmd>Telescope diagnostics<cr>", "Workspace Diagnostics" },
        f = { "<cmd>lua vim.lsp.buf.format(nil)<cr>", "Format document" },
        i = { "<cmd>Telescope lsp_implementations<cr>", "Implementations" },
        I = { "<cmd>LspInfo<cr>", "Info" },
        j = { "<cmd>Lspsaga diagnostic_jump_next<cr>", "Next Action" },
        k = { "<cmd>Lspsaga diagnostic_jump_prev<cr>", "Previous Action" },
        l = { "<cmd>Lspsaga show_line_diagnostics<cr>", "Line Diagnostics" },
        o = { "<cmd>Lspsaga outline<cr>", "Toggle Document Symbols Outline" },
        p = { "<cmd>Lspsaga hover_doc<cr>", "Preview Definition" },
        r = { "<cmd>Lspsaga lsp_finder<cr>", "Refrences" },
        R = { "<cmd>Lspsaga rename<cr>", "Rename" },
        s = { "<cmd>Telescope lsp_document_symbols<cr>", "Document Symbols" },
        S = { "<cmd>Telescope lsp_workspace_symbols<cr>", "Workspace Symbols" },
        T = { "<cmd>Lspsaga signature_help<cr>", "Signature Help" },
        u = { "<cmd>LspRestart<cr>", "Restart LSP" },
        U = { "<cmd>LspStart<cr>", "Start LSP" },

        -- Custom that need to go away
        t = {
            name = "+Rust",
            r = { "<cmd>lua require('rust-tools.runnables').runnables()<cr>", "Run" },
            d = {
                "<cmd>lua require('rust-tools.debuggables').debuggables()<cr>", "Debug"
            }
        }
    },
    s = {
        name = "+Search",
        b = { "<cmd>Telescope buffers<cr>", "Open Buffers" },
        c = { "<cmd>Telescope command_history<cr>", "Previous commands" },
        C = { "<cmd>Telescope commands<cr>", "Available commands" },
        f = { "<cmd>Telescope find_files<cr>", "Find File" },
        h = { "<cmd>Cheatsheet<cr>", "Help Tags" },
        H = { "<cmd>Telescope help_tags<cr>", "Help Tags" },
        j = { "<cmd>Telescope jumplist<cr>", "Jump List" },
        m = { "<cmd>Telescope marks<cr>", "Marks" },
        p = { "<cmd>Telescope projects<cr>", "Projects" },
        r = { "<cmd>Telescope resume<cr>", "Goto last search state" },
        R = { "<cmd>Telescope registers<cr>", "Registers" },
        t = { "<cmd>Telescope live_grep<cr>", "Text" },
        T = { "<cmd>TodoTelescope<cr>", "Todos" }
    },
    w = {
        name = "+Window",
        h = { "<C-w><C-h>", "Move to left window" },
        j = { "<C-w><C-j>", "Move to below window" },
        k = { "<C-w><C-k>", "Move to above window" },
        l = { "<C-w><C-l>", "Move to right window" },
        w = { "<C-w>w", "Move to other window" },
        x = { "<C-w>x", "Swap with other window" },
        s = { "<C-w>s", "Split window" },
        v = { "<C-w>v", "Split window vertically" },
        c = { "<C-w>q", "Close window" },
        o = { "<C-w>o", "Keep only current window" },
        t = { "<C-w>T", "Move window to a tab" },
        m = {
            name = "+Max",
            W = { "<C-w>|", "Max out width" },
            H = { "<C-w>_", "Max out hight" }
        },
        r = {
            name = "+Resize",
            l = { "<cmd>vertical resize +10<cr>", "Increase width" },
            k = { "<cmd>resize +10<cr>", "Increase height" },
            h = { "<cmd>vertical resize -10<cr>", "Decrease width" },
            j = { "<cmd>resize -10<cr>", "Decrease height" }
        },
        n = { "<C-w>=", "Normalize Windows" }
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
