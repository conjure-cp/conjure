'use strict';
import fs = require('fs');
import * as path from 'path';
var glob = require('glob');

// Case insensitive

// import * as path from 'path';
// import * as cp from 'child_process';
// import ChildProcess = cp.ChildProcess;

const { spawn } = require('child_process');
import * as vscode from 'vscode';
import { fstat } from 'fs';

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


    public static async model() {

        console.log("MODEL------------------------");

        let current = vscode.window.activeTextEditor;
        if (!current) {
            vscode.window.showErrorMessage("No active text editor!");
            return;
        }

        let doc = current.document;
        let extension = path.extname(doc.fileName);
        console.log(extension);
        if (extension !== ".essence") {
            vscode.window.showErrorMessage("This is not a model file, the extension should be .essence");
            return;
        }

        // console.log(element);
        let dir = path.dirname(doc.uri.path);

        const process = spawn('conjure', ['modelling', '--channelling=no', '--responses=1', '-o ' + dir, doc.uri.path]);

        process.stdout.on('data', (data: string) => {
            console.log(`stdout: ${data}`);
        });

        process.stderr.on('data', (data: string) => {
            console.log(`stderr: ${data}`);
        });

        process.on('close', (code: string) => {
            console.log(`child process exited with code ${code}`);
            let uri = vscode.Uri.file('/tmp/model000001.eprime');
            vscode.commands.executeCommand('vscode.openFolder', uri);
        });

    }

    public static async solve() {
        console.log("SOLVE------------------------");
        // vscode.workspace.textDocuments[0].
        let current = vscode.window.activeTextEditor;
        if (!current) {
            vscode.window.showErrorMessage("No active text editor!");
            return;
        }

        let doc = current.document;
        let extension = path.extname(doc.fileName);
        console.log(extension);
        if (extension !== ".param") {
            vscode.window.showErrorMessage("This is not a param file");
            return;
        }

        let dir = path.dirname(doc.uri.path);

        // let args = ['solve', '--channelling=no', '--responses=1', '--copy-solutions=0',
        //     '-o ' + dir, doc.uri.path, '--solver-options',
        //     '"-dumptreejson ' + dir + '"'];

        let args = ['solve', doc.uri.path, '--solver-options',
            '"-dumptreejson out.json"'];

        console.log("conjure " + args.join(" "));

        const conjureProcess = spawn('conjure', args);

        // process

        // process.stdout.on('data', (data: string) => {
        //     console.log(`stdout: ${data}`);
        // });

        conjureProcess.stderr.on('data', (data: string) => {
            console.log(`stderr: ${data}`);

            // process.stderr.write(data);
        });

        conjureProcess.on('close', (code: string) => {




            console.log(`child process exited with code ${code}`);

            // let dir = path.dirname(doc.uri.path);
            // var myPath = dir + "/*.solution";

            // glob(myPath, function (er: any, files: string[]) {

            //     files.forEach((element: string) => {
            //         console.log(element);
            //         vscode.commands.executeCommand('vscode.openFolder', vscode.Uri.file(element));
            //     });
            // });


            // let uri = vscode.Uri.file('/tmp/conjure-output/');
            // vscode.commands.executeCommand('vscode.openFolder', parentPath + "/" + );
        });




    }


    // public static async solve() {

    //     vscode.window.showInformationMessage("Solving command !!!");

    //     console.log(vscode.workspace.textDocuments);

    //     let paramFile = (vscode.workspace.textDocuments.filter(this.isParam));

    //     if (paramFile.length === 0) {
    //         vscode.window.showErrorMessage("No parameters in workspace!");
    //         return;
    //     }

    //     let modelFile = (vscode.workspace.textDocuments.filter(this.isModel));

    //     if (modelFile.length === 0) {
    //         vscode.window.showErrorMessage("No essence models in workspace!");
    //         return;
    //     }

    //     paramFile.forEach(element => {
    //         // console.log(element);
    //         let splitted = element.fileName.split("/");
    //         splitted.pop();
    //         // console.log(splitted.join("/"));
    //         let parentPath = splitted.join("/");

    //         splitted.pop();
    //         let grandParentPath =  splitted.join("/");

    //         var myPath = parentPath + "/*.solution";

    //         glob(myPath, function (er: any, files: string[]) {
    //             // console.log(files);
    //             files.forEach((element: string) => {
    //                 fs.unlinkSync(element);    
    //             });
    //         });

    //         const process = spawn('conjure', ['solve', '--channelling=no', '--responses=1',
    //          '--copy-solutions', '-o /tmp', modelFile[0].uri.path, element.uri.path]);

    //         process.stdout.on('data', (data: string) => {
    //             // console.log(`stdout: ${data}`);
    //         });

    //         process.stderr.on('data', (data: string) => {
    //             console.log(`stderr: ${data}`);
    //         });

    //         process.on('close', (code: string) => {
    //             console.log(`child process exited with code ${code}`);

    //             var myPath = grandParentPath + "/*.solution";
    //             // var myPath = "/fileFolder/**/*.txt";

    //             glob(myPath, function (er: any, files: string[]) {
    //                 // Files is an array of filenames.
    //                 // Do something with files.


    //                 files.forEach((element: string) => {
    //                     console.log(element);
    //                     vscode.commands.executeCommand('vscode.openFolder', vscode.Uri.file(element));
    //                 });
    //             });


    //             // let uri = vscode.Uri.file('/tmp/conjure-output/');
    //             // vscode.commands.executeCommand('vscode.openFolder', parentPath + "/" + );
    //         });
    //     });


    // }
}
