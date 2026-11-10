colorscheme:
# lua
''
  local function sync_theme()
    local theme = vim.o.background == "light"
      and "${colorscheme.vim-name-light or colorscheme.vim-name}"
      or "${colorscheme.vim-name}"

    if vim.g.colors_name ~= theme then
      vim.cmd.colorscheme(theme)
    end
  end

  vim.api.nvim_create_autocmd("OptionSet", {
    group = vim.api.nvim_create_augroup("ThemeAppearance", { clear = true }),
    pattern = "background",
    callback = sync_theme,
    desc = "Match colorscheme to terminal appearance",
  })

  sync_theme()
''
