require("lz.n").load({
	"nvim-lint",
	event = { "BufReadPost", "BufNewFile" },
	after = function()
		require("lint").linters_by_ft = {
			markdown = { "vale", "markdownlint" },
			nix = { "statix" },
			bash = { "shellcheck" },
			sh = { "shellcheck" },
			dockerfile = { "hadolint" },
			yaml = { "yamllint" },
		}

		local xdg_config = os.getenv("XDG_CONFIG_HOME") or vim.fn.expand("~/.config")

		require("lint").linters.markdownlint.args = {
			"--config", xdg_config .. "/markdownlint/config.json",
			"--json"
		}

		vim.api.nvim_create_autocmd({ "BufEnter", "BufWritePost" }, {
			callback = function() require("lint").try_lint() end,
		})
	end,
})
