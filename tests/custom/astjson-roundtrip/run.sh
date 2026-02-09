rm -rf conjure-output *.solution *.stats.json sample.json
conjure pretty sample.essence --output-format=astjson > sample.json
conjure solve sample.json
echo "===== 1 ====="
cat sample.essence
echo "===== 2 ====="
conjure pretty sample.json
echo "===== 3 ====="
cat *.solution
rm -rf conjure-output *.solution *.stats.json sample.json
