import * as path from 'path';
import * as vscode from 'vscode';
import fs = require('fs');
import { spawn } from 'child_process';
import { UnsupportedMediaTypeError } from 'typescript-rest/dist/server/model/errors';
import WebviewHelper from './webviewHelper';
const { exec } = require('child_process');
const createHTML = require('create-html');
const apiConstructor = require('node-object-hash');
const hasher = apiConstructor({ sort: true, coerce: true }).hash;

const collator = new Intl.Collator(undefined, { numeric: true });

export interface MinionOptions {
    findallsols: boolean;
    nodelimit: number;
}

export interface SavileRowOptions {
    optimisation: string;
}

export interface Configuration {
    modelFileName: string;
    paramFileName: string;
    savileRowOptions: SavileRowOptions;
    minionOptions: MinionOptions;
}

function getConfigHash(config: Configuration, essenceFiles: string[], paramFiles: string[]): string {

    config.modelFileName = essenceFiles.find((p) => p.includes(config.modelFileName))!;
    config.paramFileName = paramFiles.find((p) => p.includes(config.paramFileName))!;

    let obj: any = config;
    obj.modelFileContents = fs.readFileSync(config.modelFileName).toString();
    obj.paramFileContents = fs.readFileSync(config.paramFileName).toString();
    return hasher(obj);
}

export default class ConfigureHelper {


    private static context: vscode.ExtensionContext;

    /**
     * Activate the extension
     * @param context The current state of vscode
     */
    public static activate(context: vscode.ExtensionContext) {
        ConfigureHelper.context = context;
    }

    private static configToArgList(config: Configuration, hash: string): string[] {
        return ["solve", config.modelFileName, config.paramFileName, "-o", hash, "--solver-options", "\"-dumptreesql", hash + "/out.db\""];
    }

    /**
     * Launches the webview
     * @param path Path to the directory containing conjure output
     */
    public static async launch() {

        console.log(vscode.workspace.name);

        if (!vscode.workspace.name || !vscode.workspace.rootPath) {
            vscode.window.showErrorMessage("No workspace!");
            return;
        }

        let essenceFiles = (await vscode.workspace.findFiles("**/*.essence"));
        let paramFiles = (await vscode.workspace.findFiles("**/*.param"));

        const panel = vscode.window.createWebviewPanel(
            'configure', // Identifies the type of the webview. Used internally
            "Configuration Chooser", // Title of the panel displayed to the user
            vscode.ViewColumn.One, // Editor column to show the new webview panel in.
            { "enableScripts": true } // Allow scripts for the webview
        );

        panel.webview.html = this.getWebContent();

        panel.webview.onDidReceiveMessage(async (message) => {

            switch (message.command) {

                case 'init':
                    let payload = {
                        essenceFiles: essenceFiles.map((f) => vscode.workspace.asRelativePath(f.path)).sort(collator.compare),
                        paramFiles: paramFiles.map((f) => vscode.workspace.asRelativePath(f.path)).sort(collator.compare)
                    };
                    panel.webview.postMessage({ command: "files", data: payload });
                    break;

                case 'solve':

                    const hash = getConfigHash(message.data, essenceFiles.map((uri) => uri.path), paramFiles.map((uri) => uri.path));
                    // const hash = "blah";

                    let checkSums = await vscode.workspace.findFiles("**/*.conjure-checksum");
                    let matching = checkSums.filter((file) => file.path.split("/").includes(hash));

                    if (matching.length > 0) {
                        vscode.window.showInformationMessage("Already done this one!");
                        WebviewHelper.launch(path.dirname(matching[0].path));
                        return;
                    }

                    vscode.window.withProgress({
                        cancellable: true,
                        location: vscode.ProgressLocation.Notification,
                        title: 'Solving CSP ...'

                    }, async (progress, token) => {

                        var p = new Promise((resolve, reject) => {

                            const args = this.configToArgList(message.data, hash);

                            console.log(vscode.workspace.rootPath);
                            console.log("conjure " + args.join(" "));

                            const proc = spawn("conjure", args, {
                                shell: true,
                                cwd: vscode.workspace.rootPath,
                                detached: true
                            });

                            let errorMessage = "";

                            proc.stderr.on('data', (data) => {
                                errorMessage += `${data}`;
                            });

                            proc.on('close', (code) => {
                                console.log(`child process exited with code ${code}`);
                                console.error(errorMessage);
                                vscode.window.showErrorMessage(errorMessage);
                                if (errorMessage === "") {
                                    vscode.window.showInformationMessage("Done!");
                                    WebviewHelper.launch(path.join(vscode.workspace.rootPath!, hash));
                                }
                                resolve();
                            });

                            proc.on('error', (err) => {
                                console.log('Failed to start subprocess.');
                                console.error(err);
                                vscode.window.showErrorMessage("Failed to start conjure ;_;");
                                reject();
                            });

                            token.onCancellationRequested(() => {
                                process.kill(-proc.pid);
                                vscode.window.showInformationMessage("Task Cancelled");
                            });
                        });

                        return p;
                    }); // vscode.window.withProgress
                    break;
            }
        }, undefined, this.context.subscriptions);
    }

    /**
     * Returns a single html file containing all the scripts and css from internal and external resources.
     */
    private static getWebContent(): string {

        // Internal resources
        const html = vscode.Uri.file(path.join(ConfigureHelper.context.extensionPath, 'src/configuration', 'configuration.html'));
        const htmlUri = html.with({ scheme: 'vscode-resource' });
        let htmlContent = fs.readFileSync(htmlUri.path);


        // const css = vscode.Uri.file(path.join(WebviewHelper.context.extensionPath, 'src/webview', 'main.css'));
        // const cssUri = css.with({ scheme: 'vscode-resource' });
        const scriptPath = vscode.Uri.file(path.join(ConfigureHelper.context.extensionPath, 'src/configuration/ts/dist/', 'bundle.js'));
        const scriptUri = scriptPath.with({ scheme: 'vscode-resource' });

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
            // script: [jspanelJS, d3, jquery, validator, mouseTrap, canvas, fileSaver,  scriptUri],
            script: [jquery, scriptUri],
            // script: [],
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