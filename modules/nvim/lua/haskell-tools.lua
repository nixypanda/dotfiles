local haskell_local_config = {
    hls_logfile_location = vim.fn.stdpath('log') .. '/' .. 'haskell-language-server.log',
}

vim.g.haskell_tools = {
    tools = {},
    hls = {
        debug = true,
        cmd = {
            'haskell-language-server-wrapper',
            '--lsp',
            '--log-level', 'Warning',
            '--logfile', haskell_local_config.hls_logfile_location
        }
    },
    dap = {}
}
