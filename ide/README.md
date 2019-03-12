# Build Instructions

## Install minion
```
git clone https://github.com/minion/minion-private
cd minion-private
git checkout dumptreesql
cd build
mkdir build
../configure.py
(sed '$s/$/ -l sqlite3/' Makefile)  >  temp
mv temp Makefile
make
```
Now add ./minion to your path.


## Install Nim

Follow the instructions provided when executing this script.
```
curl https://nim-lang.org/choosenim/init.sh -sSf | sh

```

## Install vsce (Program that bundles vscode extensions)

```
npm install -g vsce
```

## Build the extension
```
cd conjure/ide

npm install
npm run compile

cd src/webview/ts
npm install
npm run build
cd ../../../
```
## Build the .vsix file
```
vsce package
```

## Install the extension into vscode

```
code --install-extension conjure-0.0.1.vsix
```

#Running instructions

## Start the server

```
cd src/nim
nimble c -r src/server
```

## Commands

Open up a an essence problem: 

File -> Open Folder... -> MyFolder 

Press ctrl+shift+p and type "Conjure"  
You should see a list of commands from which to choose from. 