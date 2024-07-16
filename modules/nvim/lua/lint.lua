require("lz.n").load({
	"nvim-lint",
	event = { "BufReadPre", "BufNewFile" },
	after = function()
		local lsp_mypy = require("lint").linters.mypy
		lsp_mypy.cmd = function()
			local return_code = os.execute("poetry run mypy --version 1>/dev/null 2>/dev/null")
			if return_code == 0 then return "venv-mypy" end
			return "mypy"
		end

		require("lint").linters_by_ft = {
			markdown = { "vale", "markdownlint" },
			python = { "mypy" },
			nix = { "statix" },
			bash = { "shellcheck" },
			sh = { "shellcheck" },
			dockerfile = { "hadolint" },
			yaml = { "yamllint" },
		}

		vim.api.nvim_create_autocmd({ "BufWritePost" }, {
			callback = function() require("lint").try_lint() end,
		})
	end,
})
