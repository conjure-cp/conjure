rm -rf conjure-output *.solution *.stats.json
conjure solve 501.essence 501.param
cat 501-501.solution
rm -rf conjure-output *.solution *.stats.json 501-501.solution 501-501.stats.json
