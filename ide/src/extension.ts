'use strict';
// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';
// import ConjureHelper from './conjureHelper';
import WebviewHelper from './webviewHelper';
import ConjureHelper from './conjureHelper';

// this method is called when your extension is activated
// your extension is activated the very first time the command is executed

export const LANGID = "essence";

export function activate(context: vscode.ExtensionContext) {
    console.log('Congratulations, your extension "conjure" is now active!');
    // ConjureHelper.activate(context);

    context.subscriptions.push(vscode.commands.registerCommand('conjure.model', () => {
        ConjureHelper.model();
    }));
    context.subscriptions.push(vscode.commands.registerCommand('conjure.solve', () => {
        ConjureHelper.solve();
    }));
    // ConjureHelper.solve();

    context.subscriptions.push(vscode.commands.registerCommand('conjure.vis', () => {
        WebviewHelper.activate(context);
    }));

    // WebviewHelper.activate(context);
}


// this method is called when your extension is deactivated
export function deactivate() {
}