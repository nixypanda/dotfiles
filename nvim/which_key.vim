" Map leader to which_key
nnoremap <silent> <leader> :WhichKey '<Space>'<CR>

" By default timeoutlen is 1000 ms
set timeoutlen=500

" Create map to add keys to
let g:which_key_map =  {}

" Not a fan of floating windows for this
let g:which_key_use_floating_win = 0

" Hide status line
autocmd! FileType which_key
autocmd  FileType which_key set laststatus=0 noshowmode noruler
            \| autocmd BufLeave <buffer> set laststatus=2 noshowmode ruler

" Single mappings
let g:which_key_map['.'] = [ ':e $DOTFILES'               , 'Open dotfiles' ]
let g:which_key_map[';'] = [ ':Commands'                  , 'Commands' ]
let g:which_key_map['='] = [ '<C-W>='                     , 'Balance windows' ]
let g:which_key_map['d'] = [ ':bd'                        , 'Delete buffer']
let g:which_key_map['e'] = [ ':NvimTreeToggle'            , 'Explorer' ]
let g:which_key_map['h'] = [ '<C-W>s'                     , 'Split below']
let g:which_key_map['q'] = [ 'q'                          , 'Quit' ]
let g:which_key_map['v'] = [ '<C-W>v'                     , 'Split right']
let g:which_key_map['w'] = [ 'w'                          , 'Write' ]

" Group mappings

" a is for actions
let g:which_key_map.a = {
    \ 'name' : '+actions' ,
    \ 'e' : [':NvimTreeToggle'         , 'Explorer'],
    \ 's' : [':let @/ = ""'            , 'Remove search highlight'],
    \ 't' : [':TableModeToggle'        , 'Start/Stop Table Mode'],
    \ 'w' : [':set list!'              , 'Show/hide whitespace']
    \ }


let g:which_key_map.b = {
    \ 'name' : '+buffers' ,
    \ 'A' : [':bufdo bd'            , 'Close all buffer'],
    \ 'c' : [':BufferClose'         , 'Close this buffer'],
    \ 'C' : [':w | %bd | e#'        , 'Close all other buffers'],
    \ 'd' : [':bd'                  , 'Close buffer'],
    \ 'j' : [':BufferNext'          , 'Next Buffer'],
    \ 'k' : [':BufferPrevious'      , 'Previous Buffer'],
    \ 'J' : [':BufferMoveNext'      , 'Swap with Next Buffer'],
    \ 'K' : [':BufferMovePrevious'  , 'Swap with Previous Buffer'],
    \ 'G' : [':BufferLast'          , 'Last Buffer'],
    \}

" d is for debug
let g:which_key_map.d = {
    \ 'name' : '+debug' ,
    \ 'b' : ['<Plug>VimspectorToggleBreakpoint'              , 'Breakpoint'],
    \ 'B' : ['<Plug>VimspectorToggleConditionalBreakpoint'   , 'Conditional breakpoint'],
    \ 'c' : ['<Plug>VimspectorRunToCursor'                   , 'Run to cursor'],
    \ 'd' : ['<Plug>VimspectorContinue'                      , 'Continue'],
    \ 'f' : ['<Plug>VimspectorAddFunctionBreakpoint'         , 'Function breakpoint'],
    \ 'm' : [':MaximizerToggle'                              , 'Maximize window'],
    \ 'o' : ['<Plug>VimspectorStepOver'                      , 'Step over'],
    \ 'O' : ['<Plug>VimspectorStepOut'                       , 'Step out'],
    \ 'i' : ['<Plug>VimspectorStepInto'                      , 'Step into'],
    \ 'p' : ['<Plug>VimspectorPause'                         , 'Pause'],
    \ 'r' : ['<Plug>VimspectorRestart'                       , 'Restart'],
    \ 's' : ['<Plug>VimspectorStop'                          , 'Stop'],
    \ }

let g:which_key_map.s = {
    \ 'name' : '+Search' ,
    \ 'f' : [':Telescope find_files',            'Files'],
    \ 't' : [':Telescope live_grep',             'Text (live grep)'],
    \ 'b' : [':Telescope buffers',               'Open Buffers'],
    \ 'h' : [':Telescope help_tags',             'Help tags'],
    \ 's' : [':Telescope lsp_workspace_symbols', 'Workspace symbols'],
    \ 'm' : [':Telescope marks',                 'Marks'],
    \ }

" g is for git
let g:which_key_map.g = {
    \ 'name' : '+git' ,
    \ 'a' : [':Git add .'                        , 'add all'],
    \ 'A' : [':Git add %'                        , 'add current'],
    \ 'b' : [':Git blame'                        , 'blame'],
    \ 'c' : {
        \ 'name': '+Conflict Resolution',
        \ 's' : [':Gdiffsplit!'                  , 'Start'],
        \ 'h' : [':diffget //2'                  , 'Get hunk from left'],
        \ 'l' : [':diffget //3'                  , 'Get hunk from right'],
        \ 'f' : [':Gwrite!'                      , 'Finish'],
        \ },
    \ 'd' : [':Git diff'                         , 'diff'],
    \ 'g' : [':GGrep'                            , 'grep'],
    \ 'G' : [':Gstatus'                          , 'status'],
    \ 'h' : [':GitGutterLineHighlightsToggle'    , 'highlight hunks'],
    \ 'H' : ['<Plug>(GitGutterPreviewHunk)'      , 'preview hunk'],
    \ 'j' : ['<Plug>(GitGutterNextHunk)'         , 'Next hunk'],
    \ 'k' : ['<Plug>(GitGutterPrevHunk)'         , 'Prev hunk'],
    \ 'l' : [':Git log'                          , 'log'],
    \ 'm' : [':Gdiff!'                           , 'Resolve conflicts view'],
    \ 'p' : [':Git push'                         , 'push'],
    \ 'P' : [':Git pull'                         , 'pull'],
    \ 'r' : [':GRemove'                          , 'remove'],
    \ 's' : ['<Plug>(GitGutterStageHunk)'        , 'stage hunk'],
    \ 't' : [':GitGutterSignsToggle'             , 'toggle signs'],
    \ 'u' : ['<Plug>(GitGutterUndoHunk)'         , 'undo hunk'],
    \ }

" l is for language server protocol
let g:which_key_map.l = {
    \ 'name' : '+lsp' ,
    \ 'a' : [':Lspsaga code_action'                , 'Code Action'],
    \ 'd' : [':Lspsaga hover_doc'                  , 'Preview Doc'],
    \ 'j' : [':Lspsaga diagnostic_jump_next'       , 'Next Action'],
    \ 'k' : [':Lspsaga diagnostic_jump_prev'       , 'Prev Action'],
    \ 'r' : [':Lspsaga rename'                     , 'Rename'],
    \ 'R' : [':Lspsaga lsp_finder'                 , 'Definition and Refrences'],
    \ 'e' : [':Telescope lsp_workspace_diagnostics', 'Project Errors/Warnings'],
    \ 's' : [':Lspsaga signature_help'             , 'Preview Signature'],
    \ }

let g:which_key_map.n = {
    \'name' : '+navigation',
    \'s' : ['psn', 'Swap Parameter Next'],
    \'S' : ['psp', 'Swap Parameter Previous'],
    \ }

" Register which key map
autocmd VimEnter * call which_key#register('<Space>', "g:which_key_map")
