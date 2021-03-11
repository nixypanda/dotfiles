""""""""""""""
" Navigation "
""""""""""""""

" Easier Navigation
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" Sneak config
" case insensitive sneak
let g:sneak#use_ic_scs = 1

" immediately move to the next instance of search, if you move the cursor sneak
" is back to default behavior
let g:sneak#s_next = 1
" Change the colors
highlight Sneak guifg=black guibg=#00C7DF ctermfg=black ctermbg=cyan
highlight SneakScope guifg=red guibg=yellow ctermfg=red ctermbg=yellow

" Cool prompts
let g:sneak#prompt = '🔎'
