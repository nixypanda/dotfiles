local M = {}

function M.lsp_on_attach(client, bufnr)
	-- inlay hints
	if client.server_capabilities.inlayHintProvider then vim.lsp.inlay_hint.enable() end

	-- Set keymap
	local map = function(keys, func, desc) vim.keymap.set("n", keys, func, { buffer = bufnr, desc = desc }) end

	map("gd", vim.lsp.buf.definition, "Goto Definition")
	map("gi", vim.lsp.buf.implementation, "Goto Implementation")

	local jump_to_next_error = function() vim.diagnostic.jump({ count = 1, severity = vim.diagnostic.severity.ERROR }) end
	local jump_to_prev_error = function() vim.diagnostic.jump({ count = -1, severity = vim.diagnostic.severity.ERROR }) end
	local jump_to_next_diagnostic = function() vim.diagnostic.jump({ count = 1 }) end
	local jump_to_prev_diagnostic = function() vim.diagnostic.jump({ count = -1 }) end

	-- mutations
	map("<leader>lC", vim.lsp.codelens.run, "Code Lens")
	map("<leader>lR", vim.lsp.buf.rename, "Rename")
	map("<leader>la", "<cmd>Lspsaga code_action<cr>", "Code Action") -- shows preview, telescope-ui-select does not

	-- finding stuff
	map("<leader>lO", "<cmd>Telescope lsp_document_symbols<cr>", "Toggle Document Symbols Outline")
	map("<leader>lS", "<cmd>Telescope lsp_dynamic_workspace_symbols<cr>", "Workspace Symbols")
	map("<leader>lI", "<cmd>Telescope lsp_implementations<cr>", "Implementations")
	map("<leader>li", "<cmd>Telescope lsp_incoming_calls<cr>", "Incoming Calls")
	map("<leader>lo", "<cmd>Telescope lsp_outgoing_calls<cr>", "Outgoing Calls")
	map("<leader>lr", "<cmd>Telescope lsp_references<cr>", "Refrences")
	map("<leader>ls", "<cmd>Telescope lsp_document_symbols<cr>", "Document Symbols")
	map("<leader>lp", vim.lsp.buf.hover, "Preview Definition")

	-- diagnostics
	map("<leader>lc", "<cmd>Lspsaga show_cursor_diagnostics<cr>", "Cursor Diagnostics")
	map("<leader>ld", "<cmd>Telescope diagnostics bufnr=0<cr>", "Document Diagnostics")
	map("<leader>lD", "<cmd>Telescope diagnostics<cr>", "Workspace Diagnostics")
	map("<leader>lL", "<cmd>Lspsaga show_line_diagnostics<cr>", "Line Diagnostics")
	-- diagnostics navigation
	map("<leader>lh", jump_to_prev_diagnostic, "Previous diagnostic")
	map("<leader>ll", jump_to_next_diagnostic, "Next diagnostic")
	map("<leader>lj", jump_to_next_error, "Next error diagnostic")
	map("<leader>lk", jump_to_prev_error, "Previous error diagnostic")

	-- auxiliary
	map("<leader>lI", "<cmd>LspInfo<cr>", "Info")
	map("<leader>lu", "<cmd>LspRestart<cr>", "Restart LSP")
	map("<leader>lU", "<cmd>LspStart<cr>", "Start LSP")

	-- Other mode keymap
	vim.keymap.set("v", "<leader>la", "<cmd>LspSaga code_action<cr>", { buffer = bufnr, desc = "Code [A]ction" })
end

return M
