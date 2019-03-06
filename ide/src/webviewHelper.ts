import * as path from 'path';
import * as vscode from 'vscode';
// import * as Parser from './parser';
import fs = require('fs');
import { ECONNREFUSED } from 'constants';
const request = require('request');
let createHTML = require('create-html');

export default class WebviewHelper {

    private static context: vscode.ExtensionContext;

    private static serverURL = "http://0.0.0.0:5000";

    public static handleServerError(error: any) {

        if (error.code === "ECONNREFUSED") {
            vscode.window.showErrorMessage('Could not connect to server');
        }

        console.log(error.code);
        console.log(error.errno);
        console.log(error.syscall);
        console.log(error.address);
        console.log(error.port);
        console.log(typeof error);

    }

    public static activate(context: vscode.ExtensionContext) {

        WebviewHelper.context = context;
    }


    public static launch(path: string){

            const panel = vscode.window.createWebviewPanel(
                'treeVis', // Identifies the type of the webview. Used internally
                "Tree Visualiser", // Title of the panel displayed to the user
                vscode.ViewColumn.One, // Editor column to show the new webview panel in.
                { "enableScripts": true } // Webview options. More on these later.
            );

            panel.webview.html = WebviewHelper.getWebContent();


            panel.webview.onDidReceiveMessage(message => {

                // console.log(message.command);

                switch (message.command) {

                    case 'init':
                        request(this.serverURL + '/init/' + path, { json: true }, (err: any, res: any, body: any) => {
                            if (err) {
                                this.handleServerError(err);
                            }
                            if (res.statusCode === 501){
                                vscode.window.showErrorMessage('Selected path not valid');
                            }
                            if (res.statusCode === 502){
                                vscode.window.showErrorMessage('Minion file not valid');
                            }
                            if (res.statusCode === 503){
                                vscode.window.showErrorMessage('Eprime file not valid');
                            }
                            else {
                                panel.webview.postMessage({ command: "init", data: res.body });
                            }
                        });
                        break;
                    case 'loadNodes':
                        // console.log(message.id);
                        request(this.serverURL + '/loadNodes/' +  message.start, { json: true }, (err: any, res: any, body: any) => {
                            if (err) {
                                this.handleServerError(err);
                            }
                            else {
                                panel.webview.postMessage({ command: "loadNodes", data: res.body });
                            }
                        });
                        break;
                    case 'simpleDomains':
                        // console.log(message.id);
                        request(this.serverURL + '/simpleDomains/' + message.nodeId + '/' + message.wantExpressions, { json: true }, (err: any, res: any, body: any) => {
                            if (err) {
                                this.handleServerError(err);
                            }
                            else {
                                panel.webview.postMessage({ command: "simpleDomains", data: res.body });
                            }
                        });
                        break;
                    case 'prettyDomains':
                        // console.log(message.id);
                        request(this.serverURL + '/prettyDomains/' + message.nodeId + '/' + message.wantExpressions + '/' + message.paths, { json: true }, (err: any, res: any, body: any) => {
                            if (err) {
                                this.handleServerError(err);
                            }
                            else {
                                panel.webview.postMessage({ command: "prettyDomains", data: res.body });
                            }
                        });
                        break;
                    case 'correctPath':
                        // console.log(message.id);
                        request(this.serverURL + '/correctPath', { json: true }, (err: any, res: any, body: any) => {
                            if (err) {
                                this.handleServerError(err);
                            }
                            else {
                                panel.webview.postMessage({ command: "correctPath", data: res.body });
                            }
                        });
                        break;
                    case 'longestBranchingVariable':
                        // console.log(message.id);
                        request(this.serverURL + '/longestBranchingVariable', { json: true }, (err: any, res: any, body: any) => {
                            if (err) {
                                this.handleServerError(err);
                            }
                            else {
                                panel.webview.postMessage({ command: "longestBranchingVariable", data: res.body });
                            }
                        });
                        break;
                    case 'loadCore':
                        // console.log(message.id);
                        request(this.serverURL + '/loadCore', { json: true }, (err: any, res: any, body: any) => {
                            if (err) {
                                this.handleServerError(err);
                            }
                            else {
                                panel.webview.postMessage({ command: "loadCore", data: res.body });
                            }
                        });
                        break;

                    case 'loadChildren':
                        // console.log(message.id);
                        request(this.serverURL + '/loadChildren/' + message.id, { json: true }, (err: any, res: any, body: any) => {
                            if (err) {
                                this.handleServerError(err);
                            }
                            else {
                                panel.webview.postMessage({ command: "loadChildren", data: res.body });
                            }
                        });
                        break;

                    case 'loadSet':
                        // console.log(message.id);
                        request(this.serverURL + '/loadSet/' + message.nodeId + "/" + message.path , { json: true }, (err: any, res: any, body: any) => {
                            if (err) {
                                this.handleServerError(err);
                            }
                            else {
                                panel.webview.postMessage({ command: "loadSet", data: res.body });
                            }
                        });
                        break;

                }
            }, undefined, this.context.subscriptions);
    }

    private static getWebContent(): string {

        // // Internal files
        const html = vscode.Uri.file(path.join(WebviewHelper.context.extensionPath, 'src/webview', 'main.html'));
        const htmlUri = html.with({ scheme: 'vscode-resource' });
        let htmlContent = fs.readFileSync(htmlUri.path);
        const css = vscode.Uri.file(path.join(WebviewHelper.context.extensionPath, 'src/webview', 'main.css'));
        const cssUri = css.with({ scheme: 'vscode-resource' });
        // const scriptPath = vscode.Uri.file(path.join(WebviewHelper.context.extensionPath, 'out/', 'bundle.js'));
        const scriptPath = vscode.Uri.file(path.join(WebviewHelper.context.extensionPath, 'src/webview/ts/dist/', 'bundle.js'));
        const scriptUri = scriptPath.with({ scheme: 'vscode-resource' });
        // const explorer = vscode.Uri.file(path.join(WebviewHelper.context.extensionPath, 'src/webview/scripts', 'explorer.js'));
        // const explorerUri = explorer.with({ scheme: 'vscode-resource' });
        // const treeViewUri = treeView.with({ scheme: 'vscode-resource' });

        // External scripts
        // const jsonDiff = "https://cdn.jsdelivr.net/npm/jsondiffpatch/dist/jsondiffpatch.umd.min.js";
        const jspanelCSS = "https://cdn.jsdelivr.net/npm/jspanel4@4.2.1/dist/jspanel.css";
        // const bootstrap = "https://stackpath.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap.min.css";
        const fontawesome = "https://use.fontawesome.com/releases/v5.6.3/css/all.css";
        const bootstrap = "https://stackpath.bootstrapcdn.com/bootstrap/4.2.1/css/bootstrap.min.css";

        const jspanelJS = "https://cdn.jsdelivr.net/npm/jspanel4@4.2.1/dist/jspanel.js";
        const d3 = "https://d3js.org/d3.v3.min.js";
        const jquery = "https://code.jquery.com/jquery-3.3.1.min.js";
        const validator = "https://cdn.jsdelivr.net/npm/jquery-validation@1.19.0/dist/jquery.validate.js";
        // const mouseTrap = "https://cdnjs.cloudflare.com/ajax/libs/mousetrap/1.4.6/mousetrap.js";
        const mouseTrap = "https://cdnjs.cloudflare.com/ajax/libs/mousetrap/1.6.2/mousetrap.min.js";



        var htmlFile = createHTML({
            title: 'example',
            script: [jspanelJS, d3, jquery, validator, mouseTrap, scriptUri],
            // script: [jquery, treeViewUri, explorerUri],
            scriptAsync: false,
            css: [jspanelCSS, bootstrap, fontawesome, cssUri],
            lang: 'en',
            dir: 'rtl',
            head: '<meta name="description" content="example">',
            body: htmlContent,
            favicon: 'favicon.png'
        });


        fs.writeFileSync('/home/tom/Desktop/blah.html', htmlFile, 'utf8');

        return htmlFile;

        // return(fs.readFileSync('/home/tom/conjure/ide/out/blah.html').toString());

    }
}