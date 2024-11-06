require("lz.n").load({
	"nvim-lint",
	event = { "BufReadPre", "BufNewFile" },
	after = function()
		require("lint").linters_by_ft = {
			markdown = { "vale", "markdownlint" },
			nix = { "statix" },
			bash = { "shellcheck" },
			sh = { "shellcheck" },
			dockerfile = { "hadolint" },
			yaml = { "yamllint" },
		}

		vim.api.nvim_create_autocmd({ "BufEnter", "BufWritePost" }, {
			callback = function() require("lint").try_lint() end,
		})
	end,
})
