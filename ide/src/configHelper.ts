import * as path from "path"
import * as vscode from "vscode"
import fs = require("fs")
import { spawn, ChildProcess } from "child_process"
import apiConstructor = require("node-object-hash")
import rimraf = require("rimraf")
import { noop } from "react-select/lib/utils"
const createHTML = require("create-html")
const kill = require("tree-kill")

const hasher = apiConstructor({ sort: true, coerce: true }).hash

export type RepMap = Record<string, VarRepresentation[]>

export interface RepOption {
  answer: number
  description: string
}

export interface VarRepresentation {
  name: string
  representations: RepOption[]
}

interface ToProcess {
  args: string[]
  hash: string
  config: any
  name: string
}

interface Separation {
  needToGenerate: ToProcess[]
  loadFromCache: ToProcess[]
}

export interface Cache {
  name: string
  config: any
}

export default class ConfigureHelper {
  private static context: vscode.ExtensionContext
  public static cacheFolderPath: string
  private static cacheFolderName = "vscodeExtensionCache"
  public static cacheFileName = "vscode.extensionCache.json"

  /**
   * Activate the extension
   * @param context The current state of vscode
   */
  public static activate(context: vscode.ExtensionContext) {
    ConfigureHelper.context = context
    let p = path.join(vscode.workspace.rootPath!, this.cacheFolderName)

    if (!fs.existsSync(p)) {
      fs.mkdirSync(p)
    }
    this.cacheFolderPath = p
  }

  public static makePromise(
    needToGenerate: ToProcess[],
    progress: vscode.Progress<{}>,
    token: vscode.CancellationToken
  ) {
    return new Promise((resolve, reject) => {
      const pIncrement = 5

      let doneCount = 0
      let procs: ChildProcess[] = []
      let pid2JobId: any = {}

      for (let i = 0; i < needToGenerate.length; i++) {
        const obj = needToGenerate[i]

        const proc = spawn("conjure", obj.args, {
          shell: true,
          cwd: this.cacheFolderPath,
          detached: true
        })

        procs.push(proc)

        pid2JobId[proc.pid] = i + 1

        progress.report({
          message: `${this.getProgressMessage(
            doneCount,
            needToGenerate.length,
            proc,
            pid2JobId
          )}
                             Generating models..`,
          increment: pIncrement
        })

        let errorMessage = ""

        proc.stdout.on("data", data => {
          data = data.toString()
          console.log(data.toString())

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
            })
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
            })
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
            })
          }
        })

        proc.stderr.on("data", data => {
          errorMessage += `${data}`
          console.error(`${data}`)
        })

        proc.on("close", code => {
          doneCount++

          progress.report({
            message: `${this.getProgressMessage(
              doneCount,
              needToGenerate.length,
              proc,
              pid2JobId
            )}
                                Running Minion..`,
            increment: pIncrement
          })

          fs.writeFileSync(
            path.join(this.cacheFolderPath, obj.name, this.cacheFileName),
            JSON.stringify({
              config: obj.config
            })
          )

          fs.writeFileSync(
            path.join(this.cacheFolderPath, obj.name, `${obj.hash}.hash`),
            ""
          )

          console.log(`child process exited with code ${code}`)
          console.error(errorMessage)

          if (errorMessage === "") {
            console.log("Success")
          } else {
            kill(proc.pid)
            vscode.window.showErrorMessage(
              `Config ${pid2JobId[proc.pid]} | ${errorMessage}`
            )
            rimraf.sync(path.join(vscode.workspace.rootPath!, obj.hash))
            reject()
          }

          if (doneCount === needToGenerate.length) {
            resolve()
          }
        })

        proc.on("error", err => {
          console.log("Failed to start subprocess.")
          console.error(err)
          vscode.window.showErrorMessage("Failed to start conjure ;_;")
          reject()
        })

        token.onCancellationRequested(() => {
          procs.map((proc: ChildProcess) => {
            console.log(proc.pid)
            kill(proc.pid)
          })

          console.log("canceled")

          needToGenerate.map(j =>
            rimraf.sync(path.join(this.cacheFolderPath, j.name))
          )
        })
      }
      if (needToGenerate.length === 0) {
        resolve()
      }
    }).then(() => {
      vscode.window.showInformationMessage("Done")
    })
  }

  public static async separateJobs(list: Cache[]): Promise<Separation> {
    let loadFromCache: ToProcess[] = []
    let needToGenerate: ToProcess[] = []

    for (let i = 0; i < list.length; i++) {
      const cache = list[i]

      const hash = hasher(cache.config)
      const args = this.configToArgs(cache.config, cache.name)

      const obj = {
        args: args,
        hash: hash,
        config: cache.config,
        name: cache.name
      }

      const hashes = await vscode.workspace.findFiles("**/*.hash")
      const uri = hashes.find(h => path.basename(h.path).includes(hash))

      if (uri) {
        const dirname = path.dirname(uri.path)

        if (dirname !== cache.name) {
          fs.renameSync(
            dirname,
            dirname.replace(path.basename(dirname), cache.name)
          )
        }

        loadFromCache.push(obj)
        vscode.window.showInformationMessage(
          `Loading config${i + 1} from cache...`
        )
      } else {
        console.log()
        if (fs.existsSync(path.join(this.cacheFolderPath, obj.name))) {
          rimraf.sync(path.join(this.cacheFolderPath, obj.name))
        }
        needToGenerate.push(obj)
      }
    }

    return { loadFromCache: loadFromCache, needToGenerate: needToGenerate }
  }

  public static getProgressMessage(
    doneCount: number,
    jobCount: number,
    proc: ChildProcess,
    pid2JobId: any
  ): string {
    return `[${doneCount + 1}/${jobCount}] - Config ${pid2JobId[proc.pid]} - `
  }
  public static configToArgs(config: any, name: string): string[] {
    const outputPath = path.join(this.cacheFolderPath, name)

    const fullPathToModel = path.join(
      vscode.workspace.rootPath!,
      config.essenceFile
    )
    const fullPathToParam = path.join(
      vscode.workspace.rootPath!,
      config.paramFile
    )

    let conjureOptions = [
      "solve",
      fullPathToModel,
      fullPathToParam,
      "-o",
      outputPath
    ]

    if ("conjureTime" in config) {
      conjureOptions.push(`--limit-time=${config.conjureTime}`)
    }

    if ("strategy" in config) {
      conjureOptions.push("-a")
      conjureOptions.push(config.strategy)
    }

    if ("answers" in config) {
      conjureOptions.push("-aai")
      conjureOptions.push("--channelling=no")
      conjureOptions.push("--smart-filenames")
      conjureOptions.push("--responses-representation")
      conjureOptions.push(`${config.answers.join(",")}`)
    }

    let savileRowOptions = ["--savilerow-options", '"']

    if ("optimisation" in config) {
      savileRowOptions.push(config.optimisation)
    }

    if ("symmetry" in config) {
      savileRowOptions.push(config.symmetry)
    }

    if ("translation" in config) {
      savileRowOptions.push(config.translation)
    }

    if ("srTime" in config) {
      savileRowOptions.push("-timelimit")
      savileRowOptions.push(config.srTime)
    }

    if ("cnfLimit" in config) {
      savileRowOptions.push("-cnflimit")
      savileRowOptions.push(config.cnfLimit)
    }

    savileRowOptions.push('"')

    let minionOptions = [
      "--solver-options",
      '"-dumptreesql',
      path.join(outputPath, "/out.db")
    ]

    if ("minionSwitches" in config) {
      minionOptions = minionOptions.concat(config.minionSwitches)
    }

    if ("nodeLimit" in config) {
      minionOptions.push("-nodelimit")
      minionOptions.push(config.nodeLimit)
    }

    if ("solLimit" in config) {
      minionOptions.push("-sollimit")
      minionOptions.push(config.solLimit)
    }

    if ("cpuLimit" in config) {
      minionOptions.push("-cpulimit")
      minionOptions.push(config.cpuLimit)
    }

    if ("preprocessing" in config) {
      minionOptions.push("-preprocess")
      minionOptions.push(config.preprocessing)
    }

    if ("consistency" in config) {
      minionOptions.push("-prop-node")
      minionOptions.push(config.consistency)
    }

    minionOptions.push('"')

    conjureOptions = conjureOptions
      .concat(savileRowOptions)
      .concat(minionOptions)

    console.log("conjure " + conjureOptions.join(" "))

    return conjureOptions
  }

  public static async invalidateCaches() {
    let caches = await vscode.workspace.findFiles(`**/${this.cacheFileName}`)
    caches.map(file => rimraf.sync(path.dirname(file.path)))
    vscode.window.showInformationMessage("Caches invalidated")
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
    )

    panel.webview.html = this.getWebContent()
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
    )
    const htmlUri = html.with({ scheme: "vscode-resource" })
    let htmlContent = fs.readFileSync(htmlUri.path)

    // const css = vscode.Uri.file(path.join(WebviewHelper.context.extensionPath, 'src/webview', 'main.css'));
    // const cssUri = css.with({ scheme: 'vscode-resource' });
    const scriptPath = vscode.Uri.file(
      path.join(
        ConfigureHelper.context.extensionPath,
        "dist/",
        "configBundle.js"
      )
    )

    const cssPath = vscode.Uri.file(
      path.join(
        ConfigureHelper.context.extensionPath,
        "src/config/",
        "styles.css"
      )
    )

    const scriptUri = scriptPath.with({ scheme: "vscode-resource" })
    const cssUri = cssPath.with({ scheme: "vscode-resource" })

    // External scripts
    const jspanelCSS =
      "https://cdn.jsdelivr.net/npm/jspanel4@4.2.1/dist/jspanel.css"
    const fontawesome =
      "https://use.fontawesome.com/releases/v5.6.3/css/all.css"
    const bootstrap =
      "https://stackpath.bootstrapcdn.com/bootstrap/4.2.1/css/bootstrap.min.css"
    const jspanelJS =
      "https://cdn.jsdelivr.net/npm/jspanel4@4.2.1/dist/jspanel.js"
    const d3 = "https://d3js.org/d3.v3.min.js"
    const jquery = "https://code.jquery.com/jquery-3.3.1.min.js"
    const validator =
      "https://cdn.jsdelivr.net/npm/jquery-validation@1.19.0/dist/jquery.validate.js"
    const mouseTrap =
      "https://cdnjs.cloudflare.com/ajax/libs/mousetrap/1.6.2/mousetrap.min.js"
    const canvas =
      "https://cdn.rawgit.com/eligrey/canvas-toBlob.js/f1a01896135ab378aa5c0118eadd81da55e698d8/canvas-toBlob.js"
    const fileSaver =
      "https://cdn.rawgit.com/eligrey/FileSaver.js/e9d941381475b5df8b7d7691013401e171014e89/FileSaver.min.js"

    var htmlFile = createHTML({
      title: "example",
      // script: [jspanelJS, d3, jquery, validator, mouseTrap, canvas, fileSaver,  scriptUri],
      script: [jquery, validator, scriptUri],
      // script: [],
      scriptAsync: false,
      // css: [jspanelCSS, bootstrap, fontawesome, cssUri],
      css: [cssUri, jspanelCSS, bootstrap, fontawesome],
      lang: "en",
      dir: "rtl",
      head: '<meta name="description" content="example">',
      body: htmlContent,
      favicon: "favicon.png"
    })
    return htmlFile
  }
}
