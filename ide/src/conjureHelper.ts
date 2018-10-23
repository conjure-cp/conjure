'use strict';

// import * as path from 'path';
// import * as cp from 'child_process';
// import ChildProcess = cp.ChildProcess;

const { spawn } = require('child_process');
import * as vscode from 'vscode';

const ESSENCE = "essence";

export default class ConjureHelper {

    private static diagnosticCollection: vscode.DiagnosticCollection;

    public static activate(context: vscode.ExtensionContext) {
        ConjureHelper.diagnosticCollection = vscode.languages.createDiagnosticCollection();
        vscode.workspace.onDidOpenTextDocument(ConjureHelper.lint);
        vscode.workspace.onDidSaveTextDocument(ConjureHelper.lint);

        vscode.workspace.onDidChangeTextDocument(event => {
            ConjureHelper.lint(event.document);
        });

        vscode.workspace.onDidCloseTextDocument((textDocument) => {
            ConjureHelper.diagnosticCollection.delete(textDocument.uri);
        }, null, context.subscriptions);

        vscode.workspace.textDocuments.forEach(ConjureHelper.lint);

        vscode.languages.registerDocumentFormattingEditProvider(ESSENCE, {
            provideDocumentFormattingEdits(document: vscode.TextDocument): vscode.TextEdit[] {
                const firstLine = document.lineAt(0);
                return [vscode.TextEdit.insert(firstLine.range.start, '42\n')];
            }
        });

        // context.subscriptions.push(vscode.languages.registerDocumentFormattingEditProvider(ESSENCE, {
        //         provideDocumentFormattingEdits(document: vscode.TextDocument, options: vscode.FormattingOptions, token: vscode.CancellationToken){
        //             // let textEdit = new vscode.TextEdit(new vscode.Range(new vscode.Position(0,2), new vscode.Position(0,10)), "SAD");
        //             // return [textEdit];
        //             const firstLine = document.lineAt(0);
        //             if (firstLine.text !== '42') {
        //                 return [vscode.TextEdit.insert(firstLine.range.start, '42\n')];
        //             }
        //         }
        // }
        // ));


        context.subscriptions.push(vscode.languages.registerHoverProvider(ESSENCE, {
            provideHover(document: vscode.TextDocument, position: vscode.Position, token: vscode.CancellationToken) {
                console.log(document.getText(document.getWordRangeAtPosition(position)));
                return new vscode.Hover('Iam a hover');
            }
        }));


        context.subscriptions.push(vscode.languages.registerCompletionItemProvider(ESSENCE, {

            provideCompletionItems(document: vscode.TextDocument, position: vscode.Position, token: vscode.CancellationToken, context: vscode.CompletionContext) {

                let item = new vscode.CompletionItem("A completeion suggestion", vscode.CompletionItemKind.Keyword);
                let list = new vscode.CompletionList([item]);
                return list;
            }
        }));
    }

    private static lint(document: vscode.TextDocument) {
        if (document.languageId !== ESSENCE) {
            return;
        }
        // console.log("Do linting here");

        let diagnostics: vscode.Diagnostic[] = [];

        const process = spawn('conjure', ['type-check', document.fileName]);

        if (!process.pid) {
            vscode.window.showInformationMessage('Conjure could not be found please make sure its in your path.');
            return;
        }

        // console.log("here")
        // process.stdout.on('data', (data: string) => {
        // 	console.log(`stdout: ${data}`);
        // });

        process.stderr.on('data', (data: string) => {
            console.log(`stderr: ${data}`);
            // var str = `${data}`;
            // var splitted = str.split("essence:", 2);
            // var splitAgain = splitted[1].split(":", 2);
            // let severity = vscode.DiagnosticSeverity.Error;
            // var range = new vscode.Range(Number(splitAgain[0]), Number(splitAgain[1]), Number(splitAgain[0]), 1000);
            // let diagnostic = new vscode.Diagnostic(range, "ASDA", severity);
            // diagnostics.push(diagnostic);
            // vscode.window.showInformationMessage("ADASDAS");

        });

        process.on('close', (code: string) => {
            console.log(`child process exited with code ${code}`);
            this.diagnosticCollection.set(document.uri, diagnostics);
        });
    }
}
