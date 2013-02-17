" Vim syntax file
" Language: Essence
" Maintainer: Bilal Syed Hussain
" Latest Revision: Thu 25 Oct 2012 

autocmd BufRead,BufNewFile *.essence,*.eprime,*.param,*.rule,*.repr,*.essence.out,*.essence.log,*.essence.err,*.solution,*.log setfiletype essence
autocmd FileType essence set commentstring=$\ %s
autocmd FileType essence set autoindent
