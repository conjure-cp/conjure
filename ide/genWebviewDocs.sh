
(cd src/webview/ts && ./node_modules/typedoc/bin/typedoc --out ./docs --mode file --target ES6 ./src/ )
firefox src/webview/ts/docs/index.html &