We run `conjure pretty` on *.essence files under this directory, and then
compare the generated `stdout` and `stderr` vs the expected `stdout` and
`stderr` files.

Expected `stdout` is saved as `stdout.expected`. If the file doesn't exist, `stdout` is expected to be empty. (Same for `stderr`)
