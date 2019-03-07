'use strict';
import fs = require('fs');
import * as path from 'path';
// var glob = require('glob');

// Case insensitive

// import * as path from 'path';
// import * as cp from 'child_process';
// import ChildProcess = cp.ChildProcess;

const { exec } = require('child_process');
const { spawn } = require('child_process');
import * as vscode from 'vscode';
import WebviewHelper from './webviewHelper';
// import { fstat } from 'fs';

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

        let diagnostics: vscode.Diagnostic[] = [];

        const process = spawn('conjure', ['type-check', document.fileName]);

        if (!process.pid) {
            vscode.window.showInformationMessage('Conjure could not be found please make sure its in your path.');
            return;
        }

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


    public static model() {

        vscode.window.showInformationMessage('Modelling..');
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
        console.log(dir);

        // let args = ['modelling', doc.uri.path, '--channelling=no', '--responses=1', "-o", dir];
        let args = ['modelling', doc.uri.path, "-o", dir];

        exec('conjure ' + args.join(" "), { cwd: dir }, (e: any, stdout: string, stderr: string) => {

            if (e instanceof Error) {

                // console.error(e);
                vscode.window.showErrorMessage(e.message);

                return;
                // throw e;
            }

            // console.log('stdout ', stdout);

            // console.log('stderr ', stderr);

            fs.readdir(dir, function (err, files) {
                const eprimeFiles = files.filter(el => /\.eprime$/.test(el));

                let uri = vscode.Uri.file(path.join(dir, eprimeFiles[0]));
                vscode.commands.executeCommand('vscode.openFolder', uri);


            });

        });

    }


    public static async visualisePath() {
        let folder = await vscode.window.showOpenDialog({ "canSelectFiles": false, "canSelectFolders": true });
        if (folder) {
            WebviewHelper.launch(folder[0].path);
        }
    }

    public static solveAndVisualise(wantVisualisation: boolean){
        console.log("SOLVE------------------------");
        // vscode.workspace.textDocuments[0].
        let current = vscode.window.activeTextEditor;
        if (!current) {
            vscode.window.showErrorMessage("No active text editor!");
            return;
        }

        let paramFile = current.document;
        let extension = path.extname(paramFile.fileName);
        // console.log(extension);
        if (extension !== ".param") {
            vscode.window.showErrorMessage("This is not a param file");
            return;
        }

        let dir = path.dirname(paramFile.uri.path);

        let essenceFiles = this.findEssenceFiles(dir);

        if (essenceFiles.length === 0) {

            dir = path.join(dir, "../");

            essenceFiles = this.findEssenceFiles(dir);

            if (essenceFiles.length === 0) {
                vscode.window.showErrorMessage("No essence files found in parent directory");
                return;
            }
        }

        this.deleteFiles(dir);

        if (essenceFiles.length > 1) {
            vscode.window.showErrorMessage("More than one essence file was found, aborting.");
            return;
        }

        let modelPath = path.join(dir, essenceFiles[0]);

        let args = ['solve', modelPath, paramFile.uri.path];

        if (wantVisualisation) {
            args.push('--solver-options');
            args.push('"-dumptreesql conjure-output/out.db"');
        }


        vscode.window.showInformationMessage('Solving..');

        exec('conjure ' + args.join(" "), { cwd: dir }, (e: any, stdout: string, stderr: string) => {

            if (e instanceof Error) {

                vscode.window.showErrorMessage(e.message);
                return;
            }

            if (wantVisualisation) {
                WebviewHelper.launch(path.join(dir, "conjure-output"));
            }
            else {
                let solutions = this.findSolutionFiles(dir);


                solutions.forEach(fileName => {
                    let paramFileName = (path.parse(paramFile.fileName).name);

                    if (fileName.includes(paramFileName)) {

                        let uri = vscode.Uri.file(path.join(dir, fileName));
                        vscode.commands.executeCommand('vscode.openFolder', uri);
                        vscode.window.showInformationMessage("Done!");
                    }
                });

                // vscode.window.showInformationMessage("No solution.");
            }
        });
    }

    private static findEssenceFiles(dir: string): string[] {
        let files = fs.readdirSync(dir);
        return files.filter(el => /\.essence$/.test(el));
    }

    private static findSolutionFiles(dir: string): string[] {
        let files = fs.readdirSync(dir);
        return files.filter(el => /\.solution$/.test(el));
    }

    private static deleteFiles(dir: string){
        var rimraf = require("rimraf");
        rimraf.sync(path.join(dir, "conjure-output"));
        // dir = path.join(dir, "conjure-output");
        // let files = fs.readdirSync(dir);
        // let db  = files.filter(el => /\.db$/.test(el));
        // let eprime  = files.filter(el => /\.eprime$/.test(el));
        // let minion  = files.filter(el => /\.eprime-minion$/.test(el));

        // db.forEach(fileName => {
        //     fs.unlinkSync(path.join(dir, fileName));
        // });

        // eprime.forEach(fileName => {
        //     fs.unlinkSync(path.join(dir, fileName));
        // });

        // minion.forEach(fileName => {
        //     fs.unlinkSync(path.join(dir, fileName));
        // });
    }
}
