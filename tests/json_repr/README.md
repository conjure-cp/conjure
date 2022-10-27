
We run `conjure pretty --output-format=astjson` on *.essence files under this directory, and then
compare the generated `model.json` and `stderr` vs the expected `model.json` and
`stderr` files.

Expected `model.json` is saved as `model.expected.json`. If the file doesn't exist,
`model.json` is expected to be empty. (Same for `stderr`)

Each test directory should contain only one Essence file. If the filename is
`disabled.essence` the test will be skipped.
