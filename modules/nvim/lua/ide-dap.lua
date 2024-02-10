-- General Setup
local dap, dapui = require("dap"), require("dapui")
dapui.setup()
dap.listeners.after.event_initialized["dapui_config"] = function()
    dapui.open()
end
dap.listeners.before.event_terminated["dapui_config"] = function()
    dapui.close()
end
dap.listeners.before.event_exited["dapui_config"] = function()
    dapui.close()
end

-- TODO: Find a nice way to do this, so it picks up theme colors
vim.fn.sign_define(
    "DapBreakpoint",
    { text = "⊕", texthl = "Error", linehl = "", numhl = "" }
)
vim.fn.sign_define(
    "DapStopped",
    { text = "→", texthl = "Info", linehl = "Info", numhl = "" }
)


-- Python Setup
local dap_python = require("dap-python")

dap_python.setup(require("injected").python_with_debugpy_path)
dap_python.test_runner = "pytest"


-- Go setup
require('dap-go').setup()
