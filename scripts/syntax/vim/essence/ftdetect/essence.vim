" Vim syntax file
" Language: Essence
" Maintainer: Bilal Syed Hussain
" Latest Revision: Mon 25 Feb 2013

autocmd BufRead,BufNewFile *.essence,*.eprime,*.param,*.rule,*.repr,*.essence.out,*.essence.log,*.essence.err,*.solution,*.log setfiletype essence
autocmd FileType essence set commentstring=$\ %s
autocmd FileType essence set autoindent


