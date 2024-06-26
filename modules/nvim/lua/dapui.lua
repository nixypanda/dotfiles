-- General Setup
local dap, dapui = require("dap"), require("dapui")
dapui.setup()
dap.listeners.before.attach.dapui_config = function() dapui.open() end
dap.listeners.before.launch.dapui_config = function() dapui.open() end
dap.listeners.before.event_terminated.dapui_config = function() dapui.close() end
dap.listeners.before.event_exited.dapui_config = function() dapui.close() end

-- TODO: Find a nice way to do this, so it picks up theme colors
vim.fn.sign_define("DapBreakpoint", { text = "⊕", texthl = "Error", linehl = "", numhl = "" })
vim.fn.sign_define("DapStopped", { text = "→", texthl = "Info", linehl = "Info", numhl = "" })
