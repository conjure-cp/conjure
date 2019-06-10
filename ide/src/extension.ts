'use strict';
import * as vscode from 'vscode';
import WebviewHelper from './webviewHelper';
import ConjureHelper from './conjureHelper';
import ConfigureHelper from './configHelper';
import * as server from './server/server'

export const LANGID = "essence";

/**
 * Activates extension and registers commands
 * @param context The VSCode state
 */
export function activate(context: vscode.ExtensionContext) {
    console.log('Conjure extension activated.');

    server.startServer()


    ConjureHelper.activate(context);
    WebviewHelper.activate(context);
    ConfigureHelper.activate(context);

    context.subscriptions.push(vscode.commands.registerCommand('conjure.model', () => {
        ConjureHelper.model();
    }));
    context.subscriptions.push(vscode.commands.registerCommand('conjure.solve', () => {
        ConjureHelper.solveAndVisualise(false);
    }));
    context.subscriptions.push(vscode.commands.registerCommand('conjure.solveAndVis', () => {
        ConjureHelper.solveAndVisualise(true);
    }));
    context.subscriptions.push(vscode.commands.registerCommand('conjure.vis', () => {
        ConjureHelper.launchVisualiserWithPath();
    }));
    context.subscriptions.push(vscode.commands.registerCommand('conjure.configure', () => {
        ConfigureHelper.launch();
    }));
    context.subscriptions.push(vscode.commands.registerCommand('conjure.invalidateCaches', () => {
        ConfigureHelper.invalidateCaches();
    }));
}

export function deactivate() {
}