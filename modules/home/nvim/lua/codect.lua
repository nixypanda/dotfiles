require("codect").setup({
	default_mode = "signatures",
})

local function show_mode(mode, normal_command)
	return function()
		local lib = package.loaded["diffview.lib"]
		if lib and lib.get_current_view and lib.get_current_view() then
			vim.cmd("CodectDiffview " .. mode)
		else
			vim.cmd(normal_command)
		end
	end
end

vim.keymap.set("n", "<leader>ot", show_mode("types", "CodectShow types"), { desc = "Codect: show types" })
vim.keymap.set(
	"n",
	"<leader>os",
	show_mode("signatures", "CodectShow signatures"),
	{ desc = "Codect: show signatures" }
)
vim.keymap.set("n", "<leader>ou", show_mode("source", "CodectFold full"), { desc = "Codect: show full source" })
vim.keymap.set("n", "<leader>oo", "<cmd>CodectOutline<cr>", { desc = "Codect: declaration outline" })
vim.keymap.set("n", "<leader>oz", "<cmd>CodectToggle<cr>", { desc = "Codect: toggle auto-fold" })
