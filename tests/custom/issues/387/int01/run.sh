rm -rf conjure-output && \
conjure solve -ax --smart-filenames --channelling=no *.essence --validate-solutions && \
cat conjure-output/*.solution && \
rm -rf conjure-output *.solution && \
rm -rf conjure-output
