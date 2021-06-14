local dap_python = require("dap-python")

dap_python.setup("python")
dap_python.test_runner = "pytest"

-- TODO: Find a nice way to do this, so it picks up theme colors
vim.fn.sign_define("DapBreakpoint", {text="⬤", texthl="Error", linehl="", numhl=""})
vim.fn.sign_define("DapStopped", {text="→", texthl="Info", linehl="Info", numhl=""})

require('dapui').setup()
