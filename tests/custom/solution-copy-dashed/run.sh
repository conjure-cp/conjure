rm -rf conjure-output *.solution *.stats.json
conjure solve sumplete-2023053102.essence p.param
ls -1 *.solution
conjure solve sumplete-2023053102.essence sumplete-1x1-42.param
ls -1 *.solution
rm -rf conjure-output *.solution *.stats.json
