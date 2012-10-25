" Vim syntax file
" Language: Essence
" Maintainer: Bilal Syed Hussain
" Latest Revision: Thu 25 Oct 2012 

if exists("b:current_syntax")
  finish
endif

syn keyword essenceKeywords forall allDifferent dim  maximising minimising forAll exists sum be by defined domain in find from function given image indexed intersect freq lambda letting maxNumParts maxPartSize minNumParts minOccur minPartSize of partial quantifier relation representation subset subsetEq such supset supsetEq that together new type union where branching on
syn keyword essenceTypes int bool enum true false total injective bijective surjective maxOccur minSize size numParts partSize complete maxSize regular

syn match essenceNumber '\([a-zA-Z_0-9]\)\@<!\d\+'
syn match essenceComment "$.*$"

syn keyword essenceFunc preimage parts max min range toSet toMSet toRelation toInt
syn keyword essenceKind matrix tuple set mset partition


syn match essenceLang "language ESSENCE'\? "

syn match essenceOpsArithmetic '+'
syn match essenceOpsArithmetic '\*'
syn match essenceOpsArithmetic '%'
syn match essenceOpsArithmetic '\^'
syn match essenceOpsArithmetic '/'

syn match essenceOpsLogical    '!'
syn match essenceOpsLogical    '->'
syn match essenceOpsLogical    '<->'
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
syn match essenceOther '-->'

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


"conjureLog
syn match conjureLog '^\[\a\{-}\]'
hi def link conjureLog Special

if exists('g:no_essence_conceal') || !has('conceal') || &enc != 'utf-8'
    finish
endif

if exists('g:essence_conceal')

" fancy stuff
syntax keyword esNiceOperator forAll    conceal cchar=∀
syntax keyword esNiceOperator sum       conceal cchar=∑
syntax keyword esNiceOperator intersect conceal cchar=∩
syntax keyword esNiceOperator exists    conceal cchar=∃
syntax keyword esNiceOperator in        conceal cchar=∈
syntax keyword esNiceOperator union     conceal cchar=∪

syntax match esNiceOperator "!=" conceal cchar=≠
syntax match esNiceOperator "/\\" conceal cchar=∧
syntax match esNiceOperator "\\/" conceal cchar=∨

hi link esNiceOperator Operator
hi! link Conceal Operator
setlocal conceallevel=2


endif
