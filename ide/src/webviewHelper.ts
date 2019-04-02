import * as path from 'path';
import * as vscode from 'vscode';
// import * as Parser from './parser';
import fs = require('fs');
// import { ECONNREFUSED } from 'constants';
const request = require('request');
let createHTML = require('create-html');

export default class WebviewHelper {

    private static context: vscode.ExtensionContext;

    private static serverURL = "http://0.0.0.0:5000";

    /**
     * Activate the extension
     * @param context The current state of vscode
     */
    public static activate(context: vscode.ExtensionContext) {
        WebviewHelper.context = context;
    }

    /**
     * Print error to the debug console and show error message to user in right side corner
     * @param error The error returned
     */
    public static handleConnectionError(error: any) {
        if (error.code === "ECONNREFUSED") {
            vscode.window.showErrorMessage('Could not connect to server');
        }
        error.log("code: "    + error.code);
        error.log("errno: "   + error.errno);
        error.log("syscall: " + error.syscall);
        error.log("address: " + error.address);
        error.log("port: "    + error.port);
    }

    /**
     * Launches the webview
     * @param path Path to the directory containing conjure output
     */
    public static launch(path: string, conjureCommand?: string){
            const panel = vscode.window.createWebviewPanel(
                'treeVis', // Identifies the type of the webview. Used internally
                "Tree Visualiser", // Title of the panel displayed to the user
                vscode.ViewColumn.One, // Editor column to show the new webview panel in.
                { "enableScripts": true } // Allow scripts for the webview
            );

            panel.webview.html = WebviewHelper.getWebContent();


            // Relay messages between the webview and the server.
            panel.webview.onDidReceiveMessage(message => {

                switch (message.command) {

                    case 'init':
                        request(this.serverURL + '/init/' + path, { json: true }, (err: any, res: any, body: any) => {
                            if (err) {
                                this.handleConnectionError(err);
                            }

                            if (res.body.error){
                                vscode.window.showErrorMessage(res.body.error);
                            }

                            else {
                                res.body["config"] = conjureCommand;
                                panel.webview.postMessage({ command: "init", data: res.body});
                            }
                        });
                        break;

                    case 'loadNodes':
                        request(this.serverURL + '/loadNodes/' +  message.start, { json: true }, (err: any, res: any, body: any) => {
                            if (err) {
                                this.handleConnectionError(err);
                            }
                            else {
                                panel.webview.postMessage({ command: "loadNodes", data: res.body });
                            }
                        });
                        break;

                    case 'simpleDomains':
                        request(this.serverURL + '/simpleDomains/' + message.nodeId + '/' + message.wantExpressions, { json: true }, (err: any, res: any, body: any) => {
                            if (err) {
                                this.handleConnectionError(err);
                            }
                            else {
                                panel.webview.postMessage({ command: "simpleDomains", data: res.body });
                            }
                        });
                        break;

                    case 'prettyDomains':
                        request(this.serverURL + '/prettyDomains/' + message.nodeId + '/' + message.wantExpressions + '/' + message.paths, { json: true }, (err: any, res: any, body: any) => {
                            if (err) {
                                this.handleConnectionError(err);
                            }
                            else {
                                panel.webview.postMessage({ command: "prettyDomains", data: res.body });
                            }
                        });
                        break;

                    case 'longestBranchingVariable':
                        request(this.serverURL + '/longestBranchingVariable', { json: true }, (err: any, res: any, body: any) => {
                            if (err) {
                                this.handleConnectionError(err);
                            }
                            else {
                                panel.webview.postMessage({ command: "longestBranchingVariable", data: res.body });
                            }
                        });
                        break;

                    case 'loadCore':
                        request(this.serverURL + '/loadCore', { json: true }, (err: any, res: any, body: any) => {
                            if (err) {
                                this.handleConnectionError(err);
                            }
                            else {
                                panel.webview.postMessage({ command: "loadCore", data: res.body });
                            }
                        });
                        break;

                    case 'loadSet':
                        request(this.serverURL + '/loadSet/' + message.nodeId + "/" + message.path , { json: true }, (err: any, res: any, body: any) => {
                            if (err) {
                                this.handleConnectionError(err);
                            }
                            else {
                                panel.webview.postMessage({ command: "loadSet", data: res.body });
                            }
                        });
                        break;

                }
            }, undefined, this.context.subscriptions);
    }

    /**
     * Returns a single html file containing all the scripts and css from internal and external resources.
     */
    private static getWebContent(): string {

        // Internal resources
        const html = vscode.Uri.file(path.join(WebviewHelper.context.extensionPath, 'src/webview', 'main.html'));
        const htmlUri = html.with({ scheme: 'vscode-resource' });
        let htmlContent = fs.readFileSync(htmlUri.path);

        console.log("here1")

        // const css = vscode.Uri.file(path.join(WebviewHelper.context.extensionPath, 'src/webview', 'main.css'));
        // const cssUri = css.with({ scheme: 'vscode-resource' });
        const scriptPath = vscode.Uri.file(path.join(WebviewHelper.context.extensionPath, 'src/webview/ts/dist/', 'bundle.js'));
        const scriptUri = scriptPath.with({ scheme: 'vscode-resource' });

        console.log("here2")

        // External scripts
        const jspanelCSS = "https://cdn.jsdelivr.net/npm/jspanel4@4.2.1/dist/jspanel.css";
        const fontawesome = "https://use.fontawesome.com/releases/v5.6.3/css/all.css";
        const bootstrap = "https://stackpath.bootstrapcdn.com/bootstrap/4.2.1/css/bootstrap.min.css";
        const jspanelJS = "https://cdn.jsdelivr.net/npm/jspanel4@4.2.1/dist/jspanel.js";
        const d3 = "https://d3js.org/d3.v3.min.js";
        const jquery = "https://code.jquery.com/jquery-3.3.1.min.js";
        const validator = "https://cdn.jsdelivr.net/npm/jquery-validation@1.19.0/dist/jquery.validate.js";
        const mouseTrap = "https://cdnjs.cloudflare.com/ajax/libs/mousetrap/1.6.2/mousetrap.min.js";
        const canvas = "https://cdn.rawgit.com/eligrey/canvas-toBlob.js/f1a01896135ab378aa5c0118eadd81da55e698d8/canvas-toBlob.js";
        const fileSaver = "https://cdn.rawgit.com/eligrey/FileSaver.js/e9d941381475b5df8b7d7691013401e171014e89/FileSaver.min.js";

        var htmlFile = createHTML({
            title: 'example',
            script: [jspanelJS, d3, jquery, validator, mouseTrap, canvas, fileSaver,  scriptUri],
            scriptAsync: false,
            // css: [jspanelCSS, bootstrap, fontawesome, cssUri],
            css: [jspanelCSS, bootstrap, fontawesome],
            lang: 'en',
            dir: 'rtl',
            head: '<meta name="description" content="example">',
            body: htmlContent,
            favicon: 'favicon.png'
        });
        return htmlFile;
    }
}