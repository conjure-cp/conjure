"use strict"
import * as vscode from "vscode"
import WebviewHelper from "./webviewHelper"
import ConjureHelper from "./conjureHelper"
import ConfigureHelper from "./configHelper"
import * as server from "./server/server"
import { exec, execSync, execFile } from "child_process"
import * as path from "path"
import fs = require("fs")
export const LANGID = "essence"

var fp = require("find-free-port")

function makeNimServer(context: vscode.ExtensionContext) {
  const p = path.join(context.extensionPath, "src/nim/src/server")
  if (!fs.existsSync(p)) {
    console.log("nim server NOT FOUND!!!")
    execSync(path.join(context.extensionPath, "/buildServer.sh"), {
      cwd: context.extensionPath
    })
  }
}

function startNimServer(context: vscode.ExtensionContext, port: number) {
  const p = path.join(context.extensionPath, "src/nim/src/server")
  const proc = execFile(p, [String(port)])
  proc.stdout.on("data", function(data) {
    console.error(data.toString())
  })

  console.log("nim server started", proc)
}

/**
 * Activates extension and registers commands
 * @param context The VSCode state
 */
export function activate(context: vscode.ExtensionContext) {
  console.log("Conjure extension activated.")

  makeNimServer(context)
  fp(
    3000,
    5000,
    "127.0.0.1",
    2,
    (err: any, nimServerPort: number, vscodeServePort: number) => {
      if (err) {
        console.log("Could not get a port!!")
        console.error(err)
      }

      console.log("FREEPORTs are  ", nimServerPort, vscodeServePort)
      // exec()

      startNimServer(context, nimServerPort)

      server.startServer(nimServerPort, vscodeServePort, context)

      ConjureHelper.activate(context)
      WebviewHelper.activate(context)
      ConfigureHelper.activate(context)

      context.subscriptions.push(
        vscode.commands.registerCommand("conjure.model", () => {
          ConjureHelper.model()
        })
      )
      context.subscriptions.push(
        vscode.commands.registerCommand("conjure.solve", () => {
          ConjureHelper.solveAndVisualise(false)
        })
      )
      context.subscriptions.push(
        vscode.commands.registerCommand("conjure.solveAndVis", () => {
          ConjureHelper.solveAndVisualise(true)
        })
      )
      context.subscriptions.push(
        vscode.commands.registerCommand("conjure.vis", () => {
          ConjureHelper.launchVisualiserWithPath()
        })
      )
      context.subscriptions.push(
        vscode.commands.registerCommand("conjure.configure", () => {
          ConfigureHelper.launch()
        })
      )
      context.subscriptions.push(
        vscode.commands.registerCommand("conjure.invalidateCaches", () => {
          ConfigureHelper.invalidateCaches()
        })
      )
    }
  )
}

export function deactivate() {}
