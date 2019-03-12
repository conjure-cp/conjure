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
        console.log("MODEL------------------------!!!!!");

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

        let args = ['modelling', doc.uri.path, '--channelling=no', '--responses=1', "-o", dir];

        exec('conjure ' + args.join(" "), { cwd: dir }, (e: any, stdout: string, stderr: string) => {

            if (e instanceof Error) {

                console.error(e);
                vscode.window.showErrorMessage(e.message);

                return;
                // throw e;
            }


            fs.readdir(dir, function (err, files) {
                const eprimeFiles = files.filter(el => /\.eprime$/.test(el));

                let uri = vscode.Uri.file(path.join(dir, eprimeFiles[0]));
                vscode.commands.executeCommand('vscode.open', uri);


            });

        });

    }


    public static async visualisePath() {
        let folder = await vscode.window.showOpenDialog({ "canSelectFiles": false, "canSelectFolders": true });
        if (folder) {
            WebviewHelper.launch(folder[0].path);
        }
    }


    public static solveAndVisualise(wantVisualisation: boolean) {
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

        if (essenceFiles.length > 1) {
            vscode.window.showErrorMessage("More than one essence file was found, aborting.");
            return;
        }

        let modelPath = path.join(dir, essenceFiles[0]);

        let conjureOutName = this.makeDirName(path.parse(modelPath).name, path.parse(paramFile.fileName).name)

        this.deleteSolutions(dir, conjureOutName);

        // console.log(conjureOutName)

        let args = ['solve', modelPath, paramFile.uri.path, '-o', conjureOutName];
        this.parseArgs(dir, conjureOutName, args, wantVisualisation);

        console.log(args. join(" "));

        vscode.window.showInformationMessage('Solving..');

        exec('conjure ' + args.join(" "), { cwd: dir }, (e: any, stdout: string, stderr: string) => {

            if (e instanceof Error) {
                vscode.window.showErrorMessage(e.message);
                return;
            }

            if (wantVisualisation) {
                WebviewHelper.launch(path.join(dir, conjureOutName));
            }

            else {
                let solutions = this.findSolutionFiles(path.join(dir, conjureOutName));
             
                console.log(solutions);

                if (solutions.length == 0){
                    vscode.window.showInformationMessage("No solution.");
                    return
                }

                solutions.forEach(fileName => {
                    let paramFileName = (path.parse(paramFile.fileName).name);

                    if (fileName.includes(paramFileName)) {

                        let uri = vscode.Uri.file(path.join(dir, conjureOutName, fileName));
                        console.log(uri);
                        vscode.commands.executeCommand('vscode.open', uri);
                        vscode.window.showInformationMessage("Done!");
                    }
                });
            }
        });
    }
    private static parseArgs(dir: string, conjureOutName: string, args: string[], wantVisualisation: boolean){
        // let args: string[] = []
        let files = fs.readdirSync(dir);
        let configFiles = files.filter(el => /config.json$/.test(el));

        if (configFiles.length == 0){
            vscode.window.showErrorMessage("No config files found");
            return;
        }

        if (configFiles.length > 1){
            vscode.window.showErrorMessage("More than one config file was found, aborting.");
            return;
        }

        let f : string;
        let json: any;

        try{
            f = fs.readFileSync(path.join(dir, configFiles[0] ), { encoding: 'utf8' });
            json = JSON.parse(f);
        }
        catch(e){
            vscode.window.showErrorMessage("Something went wrong parsing the config file, is it valid json?");
            vscode.window.showErrorMessage(e);
            return;
        }


        let obj: any = {}

        if (wantVisualisation){
            if (json.solveAndVisualise){
                obj = json.solveAndVisualise;
            }
        }
        else{
            if (json.solve){
                obj = json.solve;
            }
        }

        var minionOptions = {};
        if (obj.minionOptions){
            minionOptions = obj.minionOptions;
        }

        var savileRowOptions = {};
        if (obj.savileRowOptions){
            savileRowOptions = obj.savileRowOptions;
        }
        
        args.push(this.parseSavileRowArgs(savileRowOptions, wantVisualisation));
        args.push(this.parseMinionArgs(conjureOutName, minionOptions, wantVisualisation));
    }
    private static parseSavileRowArgs(savilerowOptions: any, wantVisualisation: boolean){
        let defaultSavileRowArgs: any = {};
        let mandatorySavileRowArgs: string[] = [];
        return "--savilerow-options \"" + this.getArgString(savilerowOptions, defaultSavileRowArgs, mandatorySavileRowArgs) + "\"";
    }

    private static parseMinionArgs(conjureOutName: string, minionOptions: any, wantVisualisation: boolean){
        let defaultMinionArgs: any = { "nodelimit": 1000 };
        let mandatoryMinionArgs = [];

        if (wantVisualisation){
            defaultMinionArgs["dumptreesql"] = conjureOutName + "/out.db";
            mandatoryMinionArgs.push("dumptreejson", "dumptreesql");
        }

        return "--solver-options \"" + this.getArgString(minionOptions, defaultMinionArgs, mandatoryMinionArgs) + "\"";
    }

    private static getArgString(obj: any, defaultSettings: any, mandatoryArgs: string[]){
        let argList: string[] = []
        // minionArgs.push('--solver-options')

        Object.keys(defaultSettings).forEach(function(key) {
            if (mandatoryArgs.includes(key)){
                argList.push("-" + key + " " + defaultSettings[key]);
                return;
            }

            if (!obj[key]){
                argList.push("-" + key + " " + defaultSettings[key]);
            }
        });

        Object.keys(obj).forEach(function(key) {
            if (mandatoryArgs.includes(key){
                return;
            }
            argList.push("-" + key + " " + obj[key]);
        });

        return argList.join(" ");

    }
    private static deleteSolutions(dir: string, conjureOutName: string){
        var rimraf = require("rimraf");
        // let glob = path.join(dir, conjureOutName, "*.solution");
        // console.log(glob);
        rimraf.sync(path.join(dir, conjureOutName, "*.solution"));
    }

    private static findEssenceFiles(dir: string): string[] {
        let files = fs.readdirSync(dir);
        return files.filter(el => /\.essence$/.test(el));
    }

    private static findSolutionFiles(dir: string): string[] {
        let files = fs.readdirSync(dir);
        return files.filter(el => /\.solution$/.test(el));
    }

    private static makeDirName(modelName: string, paramName: string) {
        return modelName + "-" + paramName + "-conjure-output";
    }
}
