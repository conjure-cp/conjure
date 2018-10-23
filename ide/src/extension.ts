'use strict';
// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';
// import ConjureHelper from './conjureHelper';
import Tree from './tree';

// this method is called when your extension is activated
// your extension is activated the very first time the command is executed

export const LANGID = "essence";

export function activate(context: vscode.ExtensionContext) {

    console.log('Congratulations, your extension "conjure" is now active!');
    // ConjureHelper.activate(context);
    Tree.activate(context);

    // Stuff
}


// this method is called when your extension is deactivated
export function deactivate() {
}