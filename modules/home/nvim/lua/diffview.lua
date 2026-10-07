local function git(args)
	local out = vim.fn.system(args)
	if vim.v.shell_error ~= 0 then return nil end
	return vim.trim(out)
end

local function nearest_ancestor_branch()
	local current = git({ "git", "rev-parse", "--abbrev-ref", "HEAD" })
	if not current or current == "" or current == "HEAD" then
		vim.notify("Diffview: not on a branch", vim.log.levels.WARN)
		return nil
	end

	local branches = vim.fn.systemlist({ "git", "for-each-ref", "--format=%(refname:short)", "refs/heads/" })
	if vim.v.shell_error ~= 0 then return nil end

	local best, best_dist
	for _, branch in ipairs(branches) do
		branch = vim.trim(branch)
		if branch ~= "" and branch ~= current then
			local merge_base = git({ "git", "merge-base", branch, "HEAD" })
			if merge_base then
				local dist = tonumber(git({ "git", "rev-list", "--count", merge_base .. "..HEAD" }))
				if dist and dist > 0 and (best_dist == nil or dist < best_dist) then
					best, best_dist = branch, dist
				end
			end
		end
	end

	if not best then
		vim.notify("Diffview: no ancestor branch found", vim.log.levels.WARN)
		return nil
	end
	return best
end

require("lz.n").load({
	"diffview.nvim",
	keys = {
		{ "<leader>go", "<cmd>DiffviewOpen<cr>", desc = "Open Diffview" },
		{ "<leader>gc", "<cmd>DiffviewClose<cr>", desc = "Close Diffview" },
		{ "<leader>gf", "<cmd>DiffviewFileHistory<cr>", desc = "Open File History" },
		{
			"<leader>gD",
			function()
				local base = nearest_ancestor_branch()
				if base then require("diffview").open({ base .. "...HEAD" }) end
			end,
			desc = "Diff branch against nearest ancestor",
		},
		{
			"<leader>gL",
			function()
				local base = nearest_ancestor_branch()
				if base then vim.cmd("DiffviewFileHistory --range=" .. base .. "..HEAD") end
			end,
			desc = "Commit history for branch vs nearest ancestor",
		},
	},
	after = function() require("diffview").setup({}) end,
})
