" Vim syntax file
" Language: Essence
" Maintainer: Bilal Syed Hussain
" Latest Revision: Mon 25 Feb 2013

if exists("b:did_essence_ftplugin")
  finish
endif
let b:did_essence_ftplugin = 1

command! PrettifyEssence execute ":%!conjure --mode pretty --in %"

if (! exists("*EssenceTypeCheck") )

"Essence Type checking
function! EssenceTypeCheck()
    if bufexists("__Essence_Typecheck__")
        bd __Essence_Typecheck__
    endif
    
    let fp = expand("%:p")
    let bnn = bufname("%")
    let errors = system("conjure --mode typeCheck --in " . fp . " 2>&1")

    split __Essence_Typecheck__
    resize 7
    normal! ggdG
    setlocal filetype=essence
    setlocal buftype=nofile
    call append(0, split(errors, '\v\n'))
    execute "normal \<C-w>\<C-w>"

endfunction

endif

nnoremap ,c :call EssenceTypeCheck()<CR> 

