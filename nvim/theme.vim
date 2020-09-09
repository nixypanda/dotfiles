colorscheme one


" Coc highlights
" Makes the floating window more readable
" NOTE: Really wish the theme could overwrite this
highlight CocErrorSign ctermfg=204 guifg=#E06C75
highlight CocWarningSign ctermfg=173 guifg=#D19A66
highlight Pmenu ctermbg=237 guibg=#2e323b


" Python Syntax highlight
let g:python_highlight_all = 1

autocmd BufNewFile,BufRead *.rasi set syntax=scss
