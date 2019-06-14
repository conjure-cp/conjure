import * as path from "path";
import * as vscode from "vscode";
import fs = require("fs");
import { spawn, ChildProcess } from "child_process";
import WebviewHelper from "./webviewHelper";
import apiConstructor = require("node-object-hash");
import rimraf = require("rimraf");
const createHTML = require("create-html");

const hasher = apiConstructor({ sort: true, coerce: true }).hash;
const collator = new Intl.Collator(undefined, { numeric: true });

// export interface ConjureOptions {
//     strategy: string
//     timelimit: number | undefined
// }

// export interface MinionOptions {
//     consistency: string
//     preprocessing: string
//     findallsols: boolean
//     randomiseorder: boolean
//     nodelimit: number | undefined
//     sollimit: number | undefined
//     cpulimit: number | undefined
// }

// export interface SavileRowOptions {
//     optimisation: string
//     symmetryBreaking: string
//     translation: string
//     timelimit: number | undefined
//     cnflimit: number | undefined
// }

// export interface Configuration {
//     modelFileName: string
//     paramFileName: string
//     conjureOptions: ConjureOptions
//     savileRowOptions: SavileRowOptions
//     minionOptions: MinionOptions
// }

// interface SearchJob {
//     hash: string
//     args: string[]
//     preview: string
// }

// interface CachedSearch {
//     hash: string
//     args: string[]
// }

// function saveConfiguration(){
//     vscode.workspace.getConfiguration()
// }

// function getConfigHash(config: Configuration, essenceFiles: string[], paramFiles: string[]): string {

//     config.modelFileName = essenceFiles.find((p) => p.includes(config.modelFileName))!
//     config.paramFileName = paramFiles.find((p) => p.includes(config.paramFileName))!

//     let obj: any = config
//     obj.modelFileContents = fs.readFileSync(config.modelFileName).toString()
//     obj.paramFileContents = fs.readFileSync(config.paramFileName).toString()
//     return hasher(obj)
// }

// function getConfigPreview(config: Configuration): string {
//     let res = ""
//     res += "model: {" + vscode.workspace.asRelativePath(config.modelFileName) + "} "
//     res += "param: {" + vscode.workspace.asRelativePath(config.paramFileName) + "} "
//     res += "Conjure Options: " + JSON.stringify(config.conjureOptions) + "  "
//     res += "Savilerow Options: " + JSON.stringify(config.savileRowOptions) + "  "
//     res += "Minion Options: " + JSON.stringify(config.minionOptions) + "    "
//     return res
// }

// function getProgressMessage(doneCount: number, jobCount: number, proc: ChildProcess, pid2JobId: any): string {
//     return `[${doneCount+1}/${jobCount}] - Config` + pid2JobId[proc.pid] + ' - '
// }

// async function solveAny(configs: Configuration[], essenceFiles: vscode.Uri[], paramFiles: vscode.Uri[]) {

//     let jobs: SearchJob[] = []
//     let cached: CachedSearch[] = []

//     for (let i = 0; i < configs.length; i++) {

//         const config = configs[i]

//         const hash = getConfigHash(config, essenceFiles.map((uri) => uri.path), paramFiles.map((uri) => uri.path))

//         if (jobs.map((job)=>job.hash).includes(hash) || cached.map((c)=>c.hash).includes(hash)){
//             vscode.window.showErrorMessage("Configs are the same! aborting..")
//             return
//         }

//         const args = configToArgList(config, hash)

//         let caches = await vscode.workspace.findFiles("**/*.extensionCache")
//         let matching = caches.filter((file) => file.path.split("/").includes(hash))

//         if (matching.length > 0) {
//             cached.push({ hash: hash, args: args })
//             vscode.window.showInformationMessage(`Loading config${i+1} from cache...`)
//             continue
//         }

//         const preview = getConfigPreview(config)
//         jobs.push({ hash: hash, args: args, preview: preview })
//     }

//     vscode.window.withProgress({
//         cancellable: true,
//         location: vscode.ProgressLocation.Notification,
//         title: 'Solving '

//     }, (progress, token) => {

//         var p = new Promise((resolve, _reject) => {

//             let doneCount = 0
//             let procs: ChildProcess[] = []
//             let pid2JobId: any = {}

//             for (let i = 0; i < jobs.length; i++){

//                 const job = jobs[i]

//                 const proc = spawn("conjure", job.args, {
//                     shell: true,
//                     cwd: vscode.workspace.rootPath,
//                     detached: true
//                 })

//                 pid2JobId[proc.pid] = i + 1

//                 progress.report({ message: getProgressMessage(doneCount, jobs.length, proc, pid2JobId) + "Generating models..", increment: 0 })

//                 let errorMessage = ""

//                 proc.stdout.on('data', (data) => {
//                     data = data.toString()
//                     console.log(data.toString())

//                     if (data.includes("Generating")) {

//                         progress.report(
//                             {
//                                 message: getProgressMessage(doneCount, jobs.length, proc, pid2JobId) + " Running Savilerow..",
//                                 increment: 10
//                             }
//                         )
//                     }
//                     if (data.includes("domain")) {
//                         progress.report(
//                             {
//                                 message: getProgressMessage(doneCount, jobs.length, proc, pid2JobId) + " Domain Filtering..",
//                                 increment: 10
//                             }
//                         )
//                     }
//                     if (data.includes("solver")) {
//                         progress.report(
//                             {
//                                 message: getProgressMessage(doneCount, jobs.length, proc, pid2JobId) + " Running Minion..",
//                                 increment: 10
//                             }
//                         )
//                     }
//                 })

//                 proc.stderr.on('data', (data) => {
//                     errorMessage += `${data}`
//                 })

//                 proc.on('close', (code) => {

//                     doneCount++

//                     progress.report({ message: getProgressMessage(doneCount, jobs.length, proc, pid2JobId) + " Running Minion.." , increment: 20 })

//                     fs.writeFileSync(path.join(vscode.workspace.rootPath!, job.hash, "vscode.extensionCache"), "blank")

//                     console.log(`child process exited with code ${code}`)
//                     console.error(errorMessage)
//                     if (errorMessage === "") {
//                         let command = "conjure " + job.args.join(" ")
//                         WebviewHelper.launch(path.join(vscode.workspace.rootPath!, job.hash), command)
//                     }
//                     else {
//                         vscode.window.showErrorMessage("Config " + pid2JobId[proc.pid] + " | " + errorMessage)
//                         rimraf.sync(path.join(vscode.workspace.rootPath!, job.hash))
//                     }

//                     if (doneCount === jobs.length) {
//                         resolve()
//                     }

//                 })

//                 proc.on('error', (err) => {
//                     console.log('Failed to start subprocess.')
//                     console.error(err)
//                     vscode.window.showErrorMessage("Failed to start conjure ;_;")
//                 })

//                 token.onCancellationRequested(() => {
//                     procs.map((proc: ChildProcess) => process.kill(-proc.pid))
//                     errorMessage = "Search Cancelled!"
//                     jobs.map((j) => rimraf.sync(path.join(vscode.workspace.rootPath!, j.hash)))
//                     cached.map((c) => rimraf.sync(path.join(vscode.workspace.rootPath!, c.hash)))
//                 })
//             }

//             cached.forEach(search => {
//                 let command = "conjure " + search.args.join(" ")
//                 WebviewHelper.launch(path.join(vscode.workspace.rootPath!, search.hash), command)
//             })

//             if (jobs.length === 0) {
//                 resolve()
//             }
//         }).then(() => {
//             vscode.window.showInformationMessage("Done")
//        })
//         return p
//     }) // vscode.window.withProgress
// }

// function configToArgList(config: Configuration, hash: string): string[] {

//     let conjureOptions = ["solve", config.modelFileName, config.paramFileName, "-o", hash]

//     if (config.conjureOptions.timelimit) {
//         conjureOptions.push("--limit-time=" + String(config.conjureOptions.timelimit))
//     }

//     conjureOptions.push("-a")
//     conjureOptions.push(config.conjureOptions.strategy)

//     let savileRowOptions = [
//         "--savilerow-options",
//         "\"-" + config.savileRowOptions.optimisation,
//         "-" + config.savileRowOptions.symmetryBreaking,
//     ]

//     if (config.savileRowOptions.translation !== "default") {
//         savileRowOptions.push("-" + config.savileRowOptions.translation)
//     }

//     if (config.savileRowOptions.cnflimit) {
//         savileRowOptions.push("-cnflimit")
//         savileRowOptions.push(String(config.savileRowOptions.cnflimit))
//     }

//     if (config.savileRowOptions.timelimit) {
//         savileRowOptions.push("-timelimit")
//         savileRowOptions.push(String(config.savileRowOptions.timelimit))
//     }

//     savileRowOptions.push("\"")

//     let minionOptions = ["--solver-options", "\"-dumptreesql", hash + "/out.db"]

//     if (config.minionOptions.findallsols) {
//         minionOptions.push("-findallsols")
//     }
//     if (config.minionOptions.randomiseorder) {
//         minionOptions.push("-randomiseorder")
//     }
//     if (config.minionOptions.nodelimit) {
//         minionOptions.push("-nodelimit")
//         minionOptions.push(String(config.minionOptions.nodelimit))
//     }
//     if (config.minionOptions.sollimit) {
//         minionOptions.push("-sollimit")
//         minionOptions.push(String(config.minionOptions.sollimit))
//     }
//     if (config.minionOptions.cpulimit) {
//         minionOptions.push("-cpulimit")
//         minionOptions.push(String(config.minionOptions.cpulimit))
//     }

//     if (config.minionOptions.preprocessing !== "default") {
//         minionOptions.push("-preprocess")
//         minionOptions.push(config.minionOptions.preprocessing)
//     }

//     if (config.minionOptions.consistency !== "default") {
//         minionOptions.push("-prop-node")
//         minionOptions.push(config.minionOptions.consistency)
//     }

//     minionOptions.push("\"")

//     return conjureOptions.concat(savileRowOptions).concat(minionOptions)
// }

interface ToProcess {
  args: string[];
  hash: string;
}

interface Separation {
  needToGenerate: ToProcess[];
  loadFromCache: ToProcess[];
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

  public static makePromise(
    needToGenerate: ToProcess[],
    progress: vscode.Progress<{}>,
    token: vscode.CancellationToken
  ) {
    return new Promise((resolve, reject) => {
      const pIncrement = 5;
      let doneCount = 0;
      let procs: ChildProcess[] = [];
      let pid2JobId: any = {};

      for (let i = 0; i < needToGenerate.length; i++) {
        const obj = needToGenerate[i];

        const proc = spawn("conjure", obj.args, {
          shell: true,
          cwd: vscode.workspace.rootPath,
          detached: true
        });

        pid2JobId[proc.pid] = i + 1;

        progress.report({
          message: `${this.getProgressMessage(
            doneCount,
            needToGenerate.length,
            proc,
            pid2JobId
          )}
                             Generating models..`,
          increment: pIncrement
        });

        let errorMessage = "";

        proc.stdout.on("data", data => {
          data = data.toString();
          console.log(data.toString());

          if (data.includes("Generating")) {
            progress.report({
              message: `${this.getProgressMessage(
                doneCount,
                needToGenerate.length,
                proc,
                pid2JobId
              )}
                                Running savilerow..`,
              increment: pIncrement
            });
          }
          if (data.includes("domain")) {
            progress.report({
              message: `${this.getProgressMessage(
                doneCount,
                needToGenerate.length,
                proc,
                pid2JobId
              )}
                                Domain filtering..`,
              increment: pIncrement
            });
          }
          if (data.includes("solver")) {
            progress.report({
              message: `${this.getProgressMessage(
                doneCount,
                needToGenerate.length,
                proc,
                pid2JobId
              )}
                                Running Minion..`,
              increment: pIncrement
            });
          }
        });

        proc.stderr.on("data", data => {
          errorMessage += `${data}`;
        });

        proc.on("close", code => {
          doneCount++;

          progress.report({
            message: `${this.getProgressMessage(
              doneCount,
              needToGenerate.length,
              proc,
              pid2JobId
            )}
                                Running Minion..`,
            increment: pIncrement
          });

          fs.writeFileSync(
            path.join(
              vscode.workspace.rootPath!,
              obj.hash,
              "vscode.extensionCache"
            ),
            "blank"
          );

          console.log(`child process exited with code ${code}`);
          console.error(errorMessage);
          if (errorMessage === "") {
            // let command = "conjure " + job.args.join(" ")
            // WebviewHelper.launch(path.join(vscode.workspace.rootPath!, job.hash), command)
            console.log("Success");
          } else {
            vscode.window.showErrorMessage(
              "Config " + pid2JobId[proc.pid] + " | " + errorMessage
            );
            rimraf.sync(path.join(vscode.workspace.rootPath!, obj.hash));
            reject();
          }

          if (doneCount === needToGenerate.length) {
            resolve();
          }
        });

        proc.on("error", err => {
          console.log("Failed to start subprocess.");
          console.error(err);
          vscode.window.showErrorMessage("Failed to start conjure ;_;");
          reject();
        });

        token.onCancellationRequested(() => {
          procs.map((proc: ChildProcess) => process.kill(-proc.pid));
          errorMessage = "Search Cancelled!";
          // rimraf.sync(path.join(vscode.workspace.rootPath!, hash));
          needToGenerate.map(j =>
            rimraf.sync(path.join(vscode.workspace.rootPath!, j.hash))
          );
          // cached.map((c) => rimraf.sync(path.join(vscode.workspace.rootPath!, c.hash)))
        });
      }
      if (needToGenerate.length === 0) {
        resolve();
      }
    }).then(() => {
      vscode.window.showInformationMessage("Done");
    });
  }

  public static async separateJobs(list: any): Promise<Separation> {
    let loadFromCache: ToProcess[] = [];
    let needToGenerate: ToProcess[] = [];

    for (let i = 0; i < list.length; i++) {
      const config = list[i];

      const hash = hasher(config);
      const args = this.configToArgs(config, hash);

      const caches = await vscode.workspace.findFiles("**/*.extensionCache");
      const matching = caches.filter(file =>
        file.path.split("/").includes(hash)
      );

      const obj = { args: args, hash: hash };

      if (matching.length > 0) {
        loadFromCache.push(obj);
        vscode.window.showInformationMessage(
          `Loading config${i + 1} from cache...`
        );
      } else {
        needToGenerate.push(obj);
      }
    }

    return { loadFromCache: loadFromCache, needToGenerate: needToGenerate };
  }

  public static getProgressMessage(
    doneCount: number,
    jobCount: number,
    proc: ChildProcess,
    pid2JobId: any
  ): string {
    return (
      `[${doneCount + 1}/${jobCount}] - Config` + pid2JobId[proc.pid] + " - "
    );
  }
  public static configToArgs(config: any, hash: string): string[] {
    const fullPathToModel = path.join(
      vscode.workspace.rootPath!,
      config.essenceFile
    );
    const fullPathToParam = path.join(
      vscode.workspace.rootPath!,
      config.paramFile
    );

    let conjureOptions = [
      "solve",
      fullPathToModel,
      fullPathToParam,
      "-o",
      hash
    ];

    if ("conjureTime" in config) {
      conjureOptions.push(`--limit-time=${config.conjureTime}`);
    }

    if ("strategy" in config) {
      conjureOptions.push("-a");
      conjureOptions.push(config.strategy);
    }

    let savileRowOptions = ["--savilerow-options", '"'];

    if ("optimisation" in config) {
      savileRowOptions.push(config.optimisation);
    }

    if ("symmetry" in config) {
      savileRowOptions.push(config.symmetry);
    }

    if ("translation" in config) {
      savileRowOptions.push(config.translation);
    }

    if ("srTime" in config) {
      savileRowOptions.push("-timelimit");
      savileRowOptions.push(config.srTime);
    }

    if ("cnfLimit" in config) {
      savileRowOptions.push("-cnflimit");
      savileRowOptions.push(config.cnfLimit);
    }

    savileRowOptions.push('"');

    let minionOptions = ["--solver-options", '"-dumptreesql', hash + "/out.db"];

    if ("minionSwitches" in config) {
      minionOptions = minionOptions.concat(config.minionSwitches);
    }

    if ("nodeLimit" in config) {
      minionOptions.push("-nodelimit");
      minionOptions.push(config.nodeLimit);
    }

    if ("solLimit" in config) {
      minionOptions.push("-sollimit");
      minionOptions.push(config.solLimit);
    }

    if ("cpuLimit" in config) {
      minionOptions.push("-cpulimit");
      minionOptions.push(config.cpuLimit);
    }

    if ("preprocessing" in config) {
      minionOptions.push("-preprocess");
      minionOptions.push(config.preprocessing);
    }

    if ("consistency" in config) {
      minionOptions.push("-prop-node");
      minionOptions.push(config.consistency);
    }

    minionOptions.push('"');

    conjureOptions = conjureOptions
      .concat(savileRowOptions)
      .concat(minionOptions);

    console.log("conjure " + conjureOptions.join(" "));

    return conjureOptions;
  }

  public static async invalidateCaches() {
    let caches = await vscode.workspace.findFiles("**/*.extensionCache");
    caches.map(file => rimraf.sync(path.dirname(file.path)));
    vscode.window.showInformationMessage("Caches invalidated");
  }

  /**
   * Launches the webview
   * @param path Path to the directory containing conjure output
   */
  public static async launch() {
    const panel = vscode.window.createWebviewPanel(
      "configure", // Identifies the type of the webview. Used internally
      "Configuration Chooser", // Title of the panel displayed to the user
      vscode.ViewColumn.One, // Editor column to show the new webview panel in.
      { enableScripts: true } // Allow scripts for the webview
    );

    panel.webview.html = this.getWebContent();
  }

  private static getWebContent(): string {
    // Internal resources
    // const html = vscode.Uri.file(path.join(ConfigureHelper.context.extensionPath, 'src/configuration', 'configuration.html'));
    const html = vscode.Uri.file(
      path.join(
        ConfigureHelper.context.extensionPath,
        "src/config",
        "configPage.html"
      )
    );
    const htmlUri = html.with({ scheme: "vscode-resource" });
    let htmlContent = fs.readFileSync(htmlUri.path);

    // const css = vscode.Uri.file(path.join(WebviewHelper.context.extensionPath, 'src/webview', 'main.css'));
    // const cssUri = css.with({ scheme: 'vscode-resource' });
    const scriptPath = vscode.Uri.file(
      path.join(
        ConfigureHelper.context.extensionPath,
        "dist/",
        "configBundle.js"
      )
    );
    const scriptUri = scriptPath.with({ scheme: "vscode-resource" });

    // External scripts
    const jspanelCSS =
      "https://cdn.jsdelivr.net/npm/jspanel4@4.2.1/dist/jspanel.css";
    const fontawesome =
      "https://use.fontawesome.com/releases/v5.6.3/css/all.css";
    const bootstrap =
      "https://stackpath.bootstrapcdn.com/bootstrap/4.2.1/css/bootstrap.min.css";
    const jspanelJS =
      "https://cdn.jsdelivr.net/npm/jspanel4@4.2.1/dist/jspanel.js";
    const d3 = "https://d3js.org/d3.v3.min.js";
    const jquery = "https://code.jquery.com/jquery-3.3.1.min.js";
    const validator =
      "https://cdn.jsdelivr.net/npm/jquery-validation@1.19.0/dist/jquery.validate.js";
    const mouseTrap =
      "https://cdnjs.cloudflare.com/ajax/libs/mousetrap/1.6.2/mousetrap.min.js";
    const canvas =
      "https://cdn.rawgit.com/eligrey/canvas-toBlob.js/f1a01896135ab378aa5c0118eadd81da55e698d8/canvas-toBlob.js";
    const fileSaver =
      "https://cdn.rawgit.com/eligrey/FileSaver.js/e9d941381475b5df8b7d7691013401e171014e89/FileSaver.min.js";

    var htmlFile = createHTML({
      title: "example",
      // script: [jspanelJS, d3, jquery, validator, mouseTrap, canvas, fileSaver,  scriptUri],
      script: [jquery, validator, scriptUri],
      // script: [],
      scriptAsync: false,
      // css: [jspanelCSS, bootstrap, fontawesome, cssUri],
      css: [jspanelCSS, bootstrap, fontawesome],
      lang: "en",
      dir: "rtl",
      head: '<meta name="description" content="example">',
      body: htmlContent,
      favicon: "favicon.png"
    });
    return htmlFile;
  }
}
