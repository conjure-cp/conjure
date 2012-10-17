" Vim syntax file
" Language: Essence
" Maintainer: Bilal Syed Hussain
" Latest Revision: Wed 17 Oct 2012

if exists("b:current_syntax")
  finish
endif

syn keyword essenceKeywords forall allDifferent dim toSet toMSet toRelation maximising minimising forAll exists toInt sum be by defined domain in find from function given image indexed intersect freq lambda letting maxNumParts maxPartSize minNumParts minOccur minPartSize of partial quantifier relation representation subset subsetEq such supset supsetEq that together new type union where branching on
syn keyword essenceTypes int bool enum true false total injective bijective surjective maxOccur minSize size numParts partSize complete maxSize regular

"syn match essenceNumber '[^A-Za-z]\d\+' 
syn match essenceNumber '\([a-zA-Z]\)\@<!\d\+' 
syn match essenceComment "$.*$"

syn keyword essenceFunc preimage parts max min range
syn keyword essenceKind matrix tuple set mset partition


syn match essenceLang "language ESSENCE "

syn match essenceOpsArithmetic '+'
syn match essenceOpsArithmetic '\*'
syn match essenceOpsArithmetic '%'
syn match essenceOpsArithmetic '\^'
syn match essenceOpsArithmetic '/'

syn match essenceOpsLogical    '!'
syn match essenceOpsLogical    '->'
syn match essenceOpsLogical    '\<-\>'
syn match essenceOpsLogical    '/\\'
syn match essenceOpsLogical    '\\/'
syn match essenceOpsLogical    '|'

syn match essenceOpsComparison '<=lex'
syn match essenceOpsComparison '>=lex'
syn match essenceOpsComparison '<lex'
syn match essenceOpsComparison '>lex'
syn match essenceOpsComparison '='
syn match essenceOpsComparison '!='
syn match essenceOpsComparison '<='
syn match essenceOpsComparison '>='
syn match essenceOpsComparison '<'
syn match essenceOpsComparison '>'

" Where to put this?
syn match essenceOpsComparison '-->'

syn match essenceOther '\.'
syn match essenceOther ':'
syn match essenceOther ','

let b:current_syntax = "essence"

hi def link essenceKeywords      Constant
hi def link essenceNumber        PreProc
hi def link essenceComment       Comment
hi def link essenceOpsArithmetic Statement
hi def link essenceOpsLogical    Statement
hi def link essenceOpsComparison Special
hi def link essenceOther         Delimiter
hi def link essenceTypes         Type
hi def link essenceLang          Special
hi def link essenceFunc          Function
hi def link essenceKind          Function
