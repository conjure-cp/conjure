"use strict";
import fs = require("fs");
import * as path from "path";
import * as vscode from "vscode";
import WebviewHelper from "./webviewHelper";
import { resolve } from "url";
const { execSync, exec } = require("child_process");

const ESSENCE = "essence";

export default class ConjureHelper {
  private static diagnosticCollection: vscode.DiagnosticCollection;

  /**
   * Bind helper methods to events
   * @param context  The vscode state
   */

  public static activate(context: vscode.ExtensionContext) {
    ConjureHelper.diagnosticCollection = vscode.languages.createDiagnosticCollection();
    vscode.workspace.onDidOpenTextDocument(ConjureHelper.lint);
    vscode.workspace.onDidSaveTextDocument(ConjureHelper.lint);

    vscode.workspace.onDidChangeTextDocument(event => {
      ConjureHelper.lint(event.document);
    });

    vscode.workspace.onDidCloseTextDocument(
      textDocument => {
        ConjureHelper.diagnosticCollection.delete(textDocument.uri);
      },
      null,
      context.subscriptions
    );

    vscode.workspace.textDocuments.forEach(ConjureHelper.lint);

    vscode.languages.registerDocumentFormattingEditProvider(ESSENCE, {
      provideDocumentFormattingEdits(
        document: vscode.TextDocument
      ): vscode.TextEdit[] {
        const firstLine = document.lineAt(0);
        return [vscode.TextEdit.insert(firstLine.range.start, "42\n")];
      }
    });

    context.subscriptions.push(
      vscode.languages.registerHoverProvider(ESSENCE, {
        provideHover(document: vscode.TextDocument, position: vscode.Position) {
          let command = "conjure ide " + document.fileName + " --dump-domains";
          let list = JSON.parse(execSync(command));
          let word = document.getText(
            document.getWordRangeAtPosition(position)
          );

          for (let i = 0; i < list.length; i++) {
            const current = list[i];
            if (current.name === word) {
              console.log("returning " + current);

              let r = {
                value: current.domain,
                language: "essence"
              };

              return new vscode.Hover(r);
            }
          }
          return undefined;
        }
      })
    );

    context.subscriptions.push(
      vscode.languages.registerCompletionItemProvider(ESSENCE, {
        provideCompletionItems() {
          let item = new vscode.CompletionItem(
            "A completion suggestion",
            vscode.CompletionItemKind.Keyword
          );
          let list = new vscode.CompletionList([item]);
          return list;
        }
      })
    );
  }

  /**
   * Not Fully Implemented.
   * Check if essence file is valid and able to be parsed
   * @param document  The document currently focussed on in the editor
   */
  private static lint(doc: vscode.TextDocument) {
    if (doc.languageId !== ESSENCE) {
      return;
    }

    let diagnostics: vscode.Diagnostic[] = [];
    let dir = path.dirname(doc.uri.path);
    let args: string[] = [];

    // Execute conjure linting

    let command = "conjure" + args.join(" ");

    exec(command, { cwd: dir }, (e: any) => {
      if (e instanceof Error) {
        console.error(e);
        vscode.window.showErrorMessage(e.message);
        return;
      }

      // Call conjure and get the errors

      let range = new vscode.Range(0, 0, 5, 5);
      let diagnostic = new vscode.Diagnostic(
        range,
        "This is a test",
        vscode.DiagnosticSeverity.Error
      );
      diagnostics.push(diagnostic);

      this.diagnosticCollection.set(doc.uri, diagnostics);

      console.log("linted!");
    });
  }

  /**
   * This method is called when the "model command is invoked"
   */
  public static model() {
    vscode.window.showInformationMessage("Modelling..");

    // Ensure that a text editor is active

    let current = vscode.window.activeTextEditor;
    if (!current) {
      vscode.window.showErrorMessage("No active text editor!");
      return;
    }

    // Ensure that the file to be modelled is a .essence file.

    let doc = current.document;
    let extension = path.extname(doc.fileName);

    if (extension !== ".essence") {
      vscode.window.showErrorMessage(
        "This is not a model file, the extension should be .essence"
      );
      return;
    }

    let dir = path.dirname(doc.uri.path);

    let args = [
      "modelling",
      doc.uri.path,
      "--channelling=no",
      "--responses=1,1,1,1,1,1,1",
      "-o",
      dir
    ];

    // Execute conjure modelling

    let command = " conjure " + args.join(" ");

    exec(command, { cwd: dir }, (e: any) => {
      if (e instanceof Error) {
        console.error(e);
        console.log(e);
        vscode.window.showErrorMessage(e.message);
        return;
      }

      fs.readdir(dir, (err, files) => {
        if (err) {
          console.error(err);
        }

        const eprimeFiles = files.filter(el => /\.eprime$/.test(el));
        let uri = vscode.Uri.file(path.join(dir, eprimeFiles[0]));
        vscode.commands.executeCommand("vscode.open", uri);
      });
    });
  }

  public static async launchVisualiserWithPath() {
    let folder = await vscode.window.showOpenDialog({
      canSelectFiles: false,
      canSelectFolders: true
    });
    if (folder) {
      WebviewHelper.launch(folder[0].path);
    }
  }

  /**
   * Solves the currently active param file and optionally visualises it.
   * @param wantVisualisation Whether the caller wants the visualiser to launched or not.
   */

  public static solveAndVisualise(wantVisualisation: boolean) {
    // Checks that a text editor is active

    let current = vscode.window.activeTextEditor;
    if (!current) {
      vscode.window.showErrorMessage("No active text editor!");
      return;
    }

    // Checks that currently active text editor is a param file.

    let paramFile = current.document;
    let extension = path.extname(paramFile.fileName);

    if (extension !== ".param") {
      vscode.window.showErrorMessage("This is not a param file");
      return;
    }

    // Look for essence files in the same directory as the param file

    let dir = path.dirname(paramFile.uri.path);

    let essenceFiles = this.findEssenceFilePathsCheckParentDirToo(dir);

    if (essenceFiles.length === 0) {
      vscode.window.showErrorMessage(
        "No essence files found in this or the parent directory"
      );
      return;
    }

    // Abort if there are multiple essence files

    if (essenceFiles.length > 1) {
      vscode.window.showErrorMessage(
        "More than one essence file was found, aborting."
      );
      return;
    }

    let modelPath = essenceFiles[0];

    // Create name for conjure output directory

    let conjureOutName = this.makeDirName(
      path.parse(modelPath).name,
      path.parse(paramFile.fileName).name
    );

    // Delete any old solutions so we dont confuse them with ones from this run

    this.deleteSolutions(dir, conjureOutName);

    // Command line arguments needed by conjure so solve a problem

    let args = ["solve", modelPath, paramFile.uri.path, "-o", conjureOutName];

    // Parse any arguments in the config file if there is one

    let parsedArgs = this.parseArgs(dir, conjureOutName, wantVisualisation);

    vscode.window.showInformationMessage("Solving..");

    // Call conjure

    let commandString =
      "conjure " + args.join(" ") + " " + parsedArgs.join(" ");

    exec(commandString, { cwd: dir }, (e: any) => {
      // Show error message if conjure threw an error

      if (e instanceof Error) {
        vscode.window.showErrorMessage(e.message);
        return;
      }

      // If the caller wants the visualisation then launch it

      if (wantVisualisation) {
        WebviewHelper.launch(
          path.join(dir, conjureOutName),
          parsedArgs.join(" ")
        );
      } else {
        // Find the solution files

        let solutions = this.findSolutionFiles(path.join(dir, conjureOutName));

        // If there are no solutions then say so

        if (solutions.length === 0) {
          vscode.window.showInformationMessage("No solution.");
          return;
        }

        // Open up the solution files

        solutions.forEach(fileName => {
          let paramFileName = path.parse(paramFile.fileName).name;

          if (fileName.includes(paramFileName)) {
            let uri = vscode.Uri.file(path.join(dir, conjureOutName, fileName));
            vscode.commands.executeCommand("vscode.open", uri);
          }
        });

        vscode.window.showInformationMessage("Done!");
      }
    });
  }

  /**
   * Tries to parse command line options from a config.json file if one can be found.
   * @param dir The directory the essence file was found in.
   * @param conjureOutName The conjure output directory.
   * @param args  The list of command line arguments to append to.
   * @param wantVisualisation Does the caller want us to parse the normal solve args or the ones for the vis..
   */
  private static parseArgs(
    dir: string,
    conjureOutName: string,
    wantVisualisation: boolean
  ): string[] {
    let args: string[] = [];

    let files = fs.readdirSync(dir);
    let configFiles = files.filter(el => /config.json$/.test(el));

    if (configFiles.length === 0) {
      vscode.window.showInformationMessage("No config files found");

      // If no config file is found then just use default settings.

      args.push(this.parseSavileRowArgs({}));
      args.push(this.parseMinionArgs(conjureOutName, {}, wantVisualisation));
      return args;
    }

    // Abort if multiple config files are found.

    if (configFiles.length > 1) {
      vscode.window.showErrorMessage(
        "More than one config file was found, aborting."
      );
      return args;
    }

    // Contents of the file
    let fileContents: string;

    // the parsed json
    let json: any;

    // Try to parse the config file
    try {
      fileContents = fs.readFileSync(path.join(dir, configFiles[0]), {
        encoding: "utf8"
      });
      json = JSON.parse(fileContents);
    } catch (e) {
      vscode.window.showErrorMessage(
        "Something went wrong parsing the config file, is it valid json?"
      );
      vscode.window.showErrorMessage(e);
      return args;
    }

    // Choose the correct object depending on the command

    let settings: any = {};

    if (wantVisualisation) {
      if (json.solveAndVisualise) {
        settings = json.solveAndVisualise;
      }
    } else {
      if (json.solve) {
        settings = json.solve;
      }
    }

    var minionArgs = {};
    if (settings.minionArgs) {
      minionArgs = settings.minionArgs;
    }

    var savileRowArgs = {};
    if (settings.savileRowArgs) {
      savileRowArgs = settings.savileRowArgs;
    }

    // Append the user specified args

    args.push(this.parseSavileRowArgs(savileRowArgs));
    args.push(
      this.parseMinionArgs(conjureOutName, minionArgs, wantVisualisation)
    );

    return args;
  }

  /**
   * Converts the savile row args into a string that will be understood by conjure
   * @param userArgs Args to give to savile row
   */

  private static parseSavileRowArgs(userArgs: any) {
    // No defaults for saville row

    // let defaultSavileRowArgs: any = {"O0":""};
    let defaultSavileRowArgs: any = {};
    let mandatorySavileRowArgs: string[] = [];

    let savileRowArgs = this.getArgString(
      userArgs,
      defaultSavileRowArgs,
      mandatorySavileRowArgs
    );

    if (savileRowArgs === "") {
      return "";
    }

    return '--savilerow-options "' + savileRowArgs + '"';
  }

  /**
   * @param conjureOutName Name of the conjure output directory
   * @param userArgs Args to give to minion
   * @param wantVisualisation Does the caller want to launch the tree visualiser
   */
  private static parseMinionArgs(
    conjureOutName: string,
    userArgs: any,
    wantVisualisation: boolean
  ) {
    // Default node limit to make sure we dont let minion go on searching for too long.

    let defaultMinionArgs: any = { nodelimit: 1000 };
    let mandatoryMinionArgs = [];

    // If the caller wants a visualisation then give minion the dumptreesql flag.

    if (wantVisualisation) {
      defaultMinionArgs["dumptreesql"] = conjureOutName + "/out.db";
      mandatoryMinionArgs.push("dumptreejson", "dumptreesql");
    }

    return (
      '--solver-options "' +
      this.getArgString(userArgs, defaultMinionArgs, mandatoryMinionArgs) +
      '"'
    );
  }

  /**
   * Converts the list of arguments into a list
   * @param userArgs  Args specified by the user
   * @param defaultArgs Overridable defaults
   * @param mandatoryArgs Args that cannot be overrided
   */
  private static getArgString(
    userArgs: any,
    defaultArgs: any,
    mandatoryArgs: string[]
  ) {
    let argList: string[] = [];

    // Add the defaults

    Object.keys(defaultArgs).forEach(function(key) {
      if (mandatoryArgs.includes(key)) {
        argList.push("-" + key + " " + defaultArgs[key]);
        return;
      }

      if (!userArgs[key]) {
        argList.push("-" + key + " " + defaultArgs[key]);
      }
    });

    // Add the user args

    Object.keys(userArgs).forEach(function(key) {
      if (mandatoryArgs.includes(key)) {
        return;
      }
      argList.push("-" + key + " " + userArgs[key]);
    });

    return argList.join(" ");
  }

  /**
   * Deletes solution files from the conjure output directory
   * @param dir The path to the directory of the essence file
   * @param conjureOutName The name of the conjure output directory
   */

  private static deleteSolutions(dir: string, conjureOutName: string) {
    var rimraf = require("rimraf");
    rimraf.sync(path.join(dir, conjureOutName, "*.solution"));
  }

  /**
   * Returns a list of essence files that reside in a directory.
   * @param dir The path to the directory to look in.
   */
  public static findEssenceFiles(dir: string): string[] {
    let files = fs.readdirSync(dir);
    return files.filter(el => /\.essence$/.test(el));
  }

  public static findEssenceFilePathsCheckParentDirToo(dir: string): string[] {
    let paths: string[] = [];
    let essenceFiles = this.findEssenceFiles(dir);
    if (essenceFiles.length === 0) {
      dir = path.join(dir, "../");

      essenceFiles = this.findEssenceFiles(dir);
    }

    essenceFiles.forEach((filename: string) => {
      paths.push(path.join(dir, filename));
    });

    return paths;
  }

  /**
   * Returns a list of solution files that reside in a directory.
   * @param dir The path to the dir to look in
   */
  public static findSolutionFiles(dir: string): string[] {
    let files = fs.readdirSync(dir);
    return files.filter(el => /\.solution$/.test(el));
  }

  /**
   * Returns name of the conjure output directory
   * @param modelName Name of the model file
   * @param paramName Name of the param file
   */

  private static makeDirName(modelName: string, paramName: string) {
    return modelName + "-" + paramName + "-conjure-output";
  }
}