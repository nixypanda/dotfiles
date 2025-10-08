require("lz.n").load({
	"crates.nvim",
	ft = "toml",
	after = function() require("crates").setup() end,
})
