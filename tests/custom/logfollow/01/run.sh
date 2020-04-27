rm -rf conjure-output*

# run it normally
conjure -aai --smart-filenames model.essence --responses 1,1,3 -o conjure-output-1

# follow that output model exactly on an unchanged E
conjure modelling model.essence --follow-model conjure-output-1/model_1_1_3.eprime -o conjure-output-2
diff conjure-output-1/*.eprime conjure-output-2/*.eprime

# follow the same output model exactly, on an Essence + another constraint
conjure modelling model2.essence --follow-model conjure-output-1/model_1_1_3.eprime -o conjure-output-3 \
    --responses 4       # for the extra constraint

rm -rf conjure-output*
