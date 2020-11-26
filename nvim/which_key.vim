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
let g:which_key_map['e'] = [ ':CocCommand explorer'       , 'Explorer' ]
let g:which_key_map['h'] = [ '<C-W>s'                     , 'Split below']
let g:which_key_map['q'] = [ 'q'                          , 'Quit' ]
let g:which_key_map['v'] = [ '<C-W>v'                     , 'Split right']
let g:which_key_map['w'] = [ 'w'                          , 'Write' ]

" Group mappings

" a is for actions
let g:which_key_map.a = {
            \ 'name' : '+actions' ,
            \ 'e' : [':CocCommand explorer'    , 'Explorer'],
            \ 's' : [':let @/ = ""'            , 'Remove search highlight'],
            \ 't' : [':TableModeToggle'        , 'Start/Stop Table Mode'],
            \ 'w' : [':set list!'              , 'Show/hide whitespace']
            \ }


let g:which_key_map.b = {
            \ 'name' : '+buffers' ,
            \ 'C' : [':w | %bd | e#'    , 'Close all other buffers'],
            \ 'd' : [':bd'              , 'Close buffer'],
            \ 'A' : [':bufdo bd'        , 'Close all buffer'],
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


" s is for search
let g:which_key_map.s = {
            \ 'name' : '+Search' ,
            \ '/' : [':History/'     , 'History'],
            \ ';' : [':Commands'     , 'Commands'],
            \ 'a' : [':Ag'           , 'Text: Ag'],
            \ 'B' : [':BLines'       , 'Lines in current buffer'],
            \ 'b' : [':Buffers'      , 'Open buffers'],
            \ 'c' : [':Commits'      , 'Commits'],
            \ 'C' : [':BCommits'     , 'Commits for buffer'],
            \ 'f' : [':Files'        , 'Files'],
            \ 'g' : [':GFiles'       , 'Git files'],
            \ 'G' : [':GFiles?'      , 'Modified git files'],
            \ 'h' : [':History'      , 'File history'],
            \ 'H' : [':History:'     , 'Command history'],
            \ 'l' : [':Lines'        , 'Lines'] ,
            \ 'm' : [':Marks'        , 'Marks'] ,
            \ 'M' : [':Maps'         , 'Normal maps'] ,
            \ 'p' : [':Helptags'     , 'Help tags'] ,
            \ 'P' : [':Tags'         , 'Project tags'],
            \ 's' : [':Snippets'     , 'Snippets'],
            \ 'S' : [':Colors'       , 'Color schemes'],
            \ 't' : [':Rg'           , 'Text: Rg'],
            \ 'T' : [':BTags'        , 'Buffer tags'],
            \ 'w' : [':Windows'      , 'Search windows'],
            \ 'y' : [':Filetypes'    , 'File types'],
            \ 'z' : [':FZF'          , 'FZF'],
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
            \ '.' : [':CocConfig'                          , 'config'],
            \ ';' : ['<Plug>(coc-refactor)'                , 'refactor'],
            \ 'a' : [':CocCommand actions.open'            , 'line action'],
            \ 'A' : ['<Plug>(coc-codeaction-selected)'     , 'selected action'],
            \ 'b' : [':CocNext'                            , 'next action'],
            \ 'B' : [':CocPrev'                            , 'prev action'],
            \ 'c' : [':CocList commands'                   , 'commands'],
            \ 'd' : ['<Plug>(coc-definition)'              , 'definition'],
            \ 'D' : ['<Plug>(coc-declaration)'             , 'declaration'],
            \ 'e' : [':CocList extensions'                 , 'extensions'],
            \ 'f' : ['<Plug>(coc-format-selected)'         , 'format selected'],
            \ 'F' : ['<Plug>(coc-format)'                  , 'format'],
            \ 'h' : ['<Plug>(coc-float-hide)'              , 'hide'],
            \ 'i' : ['<Plug>(coc-implementation)'          , 'implementation'],
            \ 'I' : [':CocList diagnostics'                , 'diagnostics'],
            \ 'j' : ['<Plug>(coc-float-jump)'              , 'float jump'],
            \ 'l' : ['<Plug>(coc-codelens-action)'         , 'code lens'],
            \ 'n' : ['<Plug>(coc-diagnostic-next)'         , 'next diagnostic'],
            \ 'N' : ['<Plug>(coc-diagnostic-next-error)'   , 'next error'],
            \ 'o' : ['<Plug>(coc-openlink)'                , 'open link'],
            \ 'O' : [':CocList outline'                    , 'outline'],
            \ 'p' : ['<Plug>(coc-diagnostic-prev)'         , 'prev diagnostic'],
            \ 'P' : ['<Plug>(coc-diagnostic-prev-error)'   , 'prev error'],
            \ 'q' : ['<Plug>(coc-fix-current)'             , 'quickfix'],
            \ 'r' : ['<Plug>(coc-rename)'                  , 'rename'],
            \ 'R' : ['<Plug>(coc-references)'              , 'references'],
            \ 's' : [':CocList -I symbols'                 , 'references'],
            \ 't' : ['<Plug>(coc-type-definition)'         , 'type definition'],
            \ 'u' : [':CocListResume'                      , 'resume list'],
            \ 'U' : [':CocUpdate'                          , 'update CoC'],
            \ 'v' : [':Vista!!'                            , 'tag viewer'],
            \ 'z' : [':CocDisable'                         , 'disable CoC'],
            \ 'Z' : [':CocEnable'                          , 'enable CoC'],
            \ }


" Register which key map
autocmd VimEnter * call which_key#register('<Space>', "g:which_key_map")
