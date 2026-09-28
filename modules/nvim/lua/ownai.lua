require("ownai").setup({
	default_mode = "signatures",
})

local function show_mode(mode, normal_command)
	return function()
		local lib = package.loaded["diffview.lib"]
		if lib and lib.get_current_view and lib.get_current_view() then
			vim.cmd("OwnaiDiffview " .. mode)
		else
			vim.cmd(normal_command)
		end
	end
end

vim.keymap.set("n", "<leader>ot", show_mode("types", "OwnaiShow types"), { desc = "OwnAI: show types" })
vim.keymap.set("n", "<leader>os", show_mode("signatures", "OwnaiShow signatures"), { desc = "OwnAI: show signatures" })
vim.keymap.set("n", "<leader>ou", show_mode("source", "OwnaiFold full"), { desc = "OwnAI: show full source" })
vim.keymap.set("n", "<leader>oo", "<cmd>OwnaiOutline<cr>", { desc = "OwnAI: declaration outline" })
vim.keymap.set("n", "<leader>oz", "<cmd>OwnaiToggle<cr>", { desc = "OwnAI: toggle auto-fold" })
