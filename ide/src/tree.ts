import * as path from 'path';
import * as vscode from 'vscode';
import Parser from './parser';
import fs = require('fs');
let createHTML = require('create-html');

export default class Tree {

    // TODO

    // Focus to root node after minimising
    // Focus to deepest child.

    private static context: vscode.ExtensionContext;


    // private static async getFile() {

    //     let file: any;

    //     while (!file) {
    //         file = await vscode.window.showOpenDialog({});

    //         if (file) {
    //             let path = file[0].path;
    //             console.log(path);

    //             if (path.split(".", 2)[1] == "json"){
    //                 return (Parser.parseJson(fs.readFileSync(file[0].path, 'utf8')));
    //             }
    //             else if (path.split(".", 2)[1] == "sqlite"){
    //                 return (Parser.parseDB(fs.readFileSync(file[0].path, 'utf8')));
    //             }
    //             else{
    //                 vscode.window.showErrorMessage("Unexpected file format! only json and sqlite are supported!");
    //             }
    //         }
    //         else {
    //             vscode.window.showErrorMessage("There was an error please try again");
    //         }
    //     }
    // }


    public static async activate(context: vscode.ExtensionContext) {

        Tree.context = context;

        let essence = (fs.readFileSync("/home/tom/Downloads/example.essence", 'utf8'));
        let eprime = (fs.readFileSync("/home/tom/Downloads/conjure-output/model000001.eprime", 'utf8'));
        let minion = (fs.readFileSync("/home/tom/Downloads/conjure-output/model000001.eprime-minion", 'utf8'));
        let parser = new Parser(fs.readFileSync("/home/tom/Downloads/out.json", 'utf8'), essence, eprime, minion);
        let contents = parser.parseJson();
        // return;


        // Get path to resource on disk

        // let contents = await this.getFile();

        const panel = vscode.window.createWebviewPanel(
            'catCoding', // Identifies the type of the webview. Used internally
            "Cat Coding", // Title of the panel displayed to the user
            vscode.ViewColumn.One, // Editor column to show the new webview panel in.
            { "enableScripts": true } // Webview options. More on these later.
        );

        panel.webview.html = Tree.getWebContent();


        panel.webview.onDidReceiveMessage(message => {
            switch (message.command) {
                case 'ready':
                    vscode.window.showErrorMessage(message.text);
                    panel.webview.postMessage(contents);
            }
        }, undefined, context.subscriptions);


        //     panel.webview.postMessage({ message: contents });

        // context.subscriptions.push(vscode.commands.registerCommand('catCoding.start', () => {

        //     panel.webview.postMessage({ message: contents });

        // }));
    }

    private static getWebContent(): string {
        
        // Internal files
        const html = vscode.Uri.file(path.join(Tree.context.extensionPath, 'src/tree', 'main.html'));
        const htmlUri = html.with({ scheme: 'vscode-resource' });
        let htmlContent = fs.readFileSync(htmlUri.path);
        const css = vscode.Uri.file(path.join(Tree.context.extensionPath, 'src/tree', 'main.css'));
        const cssUri = css.with({ scheme: 'vscode-resource' });
        const scriptPath = vscode.Uri.file(path.join(Tree.context.extensionPath, 'src/tree', 'main.js'));
        const scriptUri = scriptPath.with({ scheme: 'vscode-resource' });
        const explorer = vscode.Uri.file(path.join(Tree.context.extensionPath, 'src/tree', 'explorer.js'));
        const explorerUri = explorer.with({ scheme: 'vscode-resource' });
        const treeView = vscode.Uri.file(path.join(Tree.context.extensionPath, 'node_modules/bootstrap-treeview/public/js', 'bootstrap-treeview.js'));
        const treeViewUri = treeView.with({ scheme: 'vscode-resource' });

        // External scripts
        const jspanelCSS = "https://cdn.jsdelivr.net/npm/jspanel4@4.2.1/dist/jspanel.css";
        const bootstrap = "https://stackpath.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap.min.css";
        const jspanelJS ="https://cdn.jsdelivr.net/npm/jspanel4@4.2.1/dist/jspanel.js";
        const d3 = "https://d3js.org/d3.v3.min.js";
        const jquery ="http://code.jquery.com/jquery-2.1.3.min.js";



        var htmlFile = createHTML({
            title: 'example',
            script: [jspanelJS, d3, jquery, treeViewUri, scriptUri] ,
            // script: [jquery, treeViewUri, explorerUri],
            scriptAsync: false,
            css: [jspanelCSS, bootstrap, cssUri],
            lang: 'en',
            dir: 'rtl',
            head: '<meta name="description" content="example">',
            body: htmlContent,
            favicon: 'favicon.png'
        });

        return htmlFile;

    }
}