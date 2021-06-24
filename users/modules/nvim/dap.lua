-- Python Setup
local dap_python = require("dap-python")

dap_python.setup("python")
dap_python.test_runner = "pytest"

-- TODO: Find a nice way to do this, so it picks up theme colors
vim.fn.sign_define("DapBreakpoint",
                   {text = "⬤", texthl = "Error", linehl = "", numhl = ""})
vim.fn.sign_define("DapStopped",
                   {text = "→", texthl = "Info", linehl = "Info", numhl = ""})

-- Rust Setup
local dap = require('dap')
dap.adapters.lldb = {
    type = 'executable',
    command = 'lldb-vscode', -- adjust as needed
    name = "lldb"
}

dap.configurations.rust = {
    {
        name = "Launch",
        type = "lldb",
        request = "launch",
        program = function()
            return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
        end,
        cwd = '${workspaceFolder}',
        stopOnEntry = false,
        args = {},

        -- if you change `runInTerminal` to true, you might need to change the yama/ptrace_scope setting:
        --
        --    echo 0 | sudo tee /proc/sys/kernel/yama/ptrace_scope
        --
        -- Otherwise you might get the following error:
        --
        --    Error on launch: Failed to attach to the target process
        --
        -- But you should be aware of the implications:
        -- https://www.kernel.org/doc/html/latest/admin-guide/LSM/Yama.html
        runInTerminal = false
    }
}

-- General Setup
require('dapui').setup()
