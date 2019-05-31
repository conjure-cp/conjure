import * as path from 'path';
import * as vscode from 'vscode';
import fs = require('fs');
import { spawn } from 'child_process';
const { exec } = require('child_process');
const createHTML = require('create-html');

const collator = new Intl.Collator(undefined, { numeric: true });

export interface MinionOptions {
    findallsols: boolean;
    nodelimit: number;
}

export interface SavileRowOptions {
    optimisation: string;
}

export interface Configuration {
    modelFile: string;
    paramFile: string;
    savileRowOptions: SavileRowOptions;
    minionOptions: MinionOptions;
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


    private static configurationToCommand(config: Configuration): string {
        let command = "conjure solve " + config.modelFile + " " + config.paramFile;
        return command;
    }

    /**
     * Launches the webview
     * @param path Path to the directory containing conjure output
     */
    public static async launch() {

        console.log(vscode.workspace.name);

        if (!vscode.workspace.name) {
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

        panel.webview.onDidReceiveMessage(message => {

            switch (message.command) {

                case 'init':
                    let payload = {
                        essenceFiles: essenceFiles.map((f) => vscode.workspace.asRelativePath(f.path)).sort(collator.compare),
                        paramFiles: paramFiles.map((f) => vscode.workspace.asRelativePath(f.path)).sort(collator.compare)
                    };
                    panel.webview.postMessage({ command: "files", data: payload });
                    break;

                case 'solve':
                    // console.log(message.data);

                    // panel.dispose();

                    vscode.window.withProgress({
                        cancellable: true,
                        location: vscode.ProgressLocation.Notification,
                        title: 'Solving CSP ...'
                        // }, (progress: Progress<{increment: number, message:string}, token:vscode.CancellationToken) => {
                    }, async (progress, token) => {

                        let commandString = this.configurationToCommand(message.data);

                        var p = new Promise(resolve => {

                            // const proc = spawn("conjure solve", ["set_partition_full-models/set_partition_full.essence", "set_partition_full-params/36-3.param"], {
                            const proc = spawn("conjure", ["solve", "set_partition_full-models/set_partition_full.essence", "set_partition_full-params/36-3.param"], {
                                cwd: vscode.workspace.rootPath,
                                detached: true
                            });

                            proc.stdout.on('data', (data) => {
                                console.log(`stdout: ${data}`);
                            });

                            proc.stderr.on('data', (data) => {
                                console.log(`stderr: ${data}`);
                            });

                            proc.on('close', (code) => {
                                console.log(`child process exited with code ${code}`);
                                resolve();
                            });

                            proc.on('error', (err) => {
                                console.log('Failed to start subprocess.');
                                console.error(err);
                            });

                            console.log(proc.pid);

                            // let proc = exec(commandString,
                            //     {
                            //         cwd: vscode.workspace.rootPath, detached: true
                            //     }, 
                            //     (error: any, stdout: any, sterr: any) => {
                            //         if (error) {
                            //             console.error(error);
                            //             vscode.window.showErrorMessage(error.message);
                            //             resolve();
                            //         }
                            //         vscode.window.showInformationMessage("DONE!");
                            //         resolve();
                            //     });


                            token.onCancellationRequested(() => {
                                vscode.window.showInformationMessage("Task Cancelled");
                                process.kill(-proc.pid);
                                // proc.kill();
                                // proc.kill();
                                // proc.kill("SIGINT");
                                // proc.kill("SIGINT");
                                // proc.kill("SIGINT");
                                // console.log(proc.killed);
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