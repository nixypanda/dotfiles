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

" FZF config

let $FZF_DEFAULT_OPTS = '--layout=reverse --info=inline'
let $FZF_DEFAULT_COMMAND="rg --files --hidden"
let g:fzf_tags_command = 'rg --files | ctags -R --links=no -L -'

" Floating layout
let g:fzf_layout = 
            \ { 'up':'~90%',
            \ 'window': {
            \ 'width': 0.8,
            \ 'height': 0.8,
            \ 'yoffset':0.5,
            \ 'xoffset': 0.5,
            \ 'highlight': 'Todo',
            \ 'border': 'sharp' }
            \ }

" Customize fzf colors to match your color scheme
let g:fzf_colors =
            \ { 'fg':      ['fg', 'Normal'],
            \ 'bg':      ['bg', 'Normal'],
            \ 'hl':      ['fg', 'Comment'],
            \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
            \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
            \ 'hl+':     ['fg', 'Statement'],
            \ 'info':    ['fg', 'PreProc'],
            \ 'border':  ['fg', 'Ignore'],
            \ 'prompt':  ['fg', 'Conditional'],
            \ 'pointer': ['fg', 'Exception'],
            \ 'marker':  ['fg', 'Keyword'],
            \ 'spinner': ['fg', 'Label'],
            \ 'header':  ['fg', 'Comment'] }


"Get Files
command! -bang -nargs=? -complete=dir Files
            \ call fzf#vim#files(<q-args>, fzf#vim#with_preview({'options': ['--layout=reverse', '--info=inline']}), <bang>0)
