"use strict"
import * as vscode from "vscode"
import WebviewHelper from "./extension/src/webviewHelper"
import ConjureHelper from "./extension/src/conjureHelper"
import ConfigureHelper from "./extension/src/configHelper"
import * as server from "./server/server"
import { exec, execSync, execFile } from "child_process"
import * as path from "path"
import fs = require("fs")
export const LANGID = "essence"

var fp = require("find-free-port")

function makeNimServer(_context: vscode.ExtensionContext) {
  // console.log(
  //   execSync(path.join(context.extensionPath, "/buildServer.sh"), {
  //     cwd: context.extensionPath
  //   }).toString()
  // )
}

function startNimServer(context: vscode.ExtensionContext, port: number) {
  const proc = exec(`./runServer.sh ${port}`, { cwd: context.extensionPath })
  proc.stdout.on("data", function (data) {
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

      nimServerPort = 5000
      vscodeServePort = 4000

      console.log("FREEPORTs are  ", nimServerPort, vscodeServePort)

      // startNimServer(context, nimServerPort)

      server.startServer(nimServerPort, vscodeServePort, context)

      ConjureHelper.activate(context)
      WebviewHelper.activate(context)
      ConfigureHelper.activate(context)

      context.subscriptions.push(
        //   vscode.commands.registerCommand("conjure.model", () => {
        //     ConjureHelper.model()
        //   })
        // )
        // context.subscriptions.push(
        //   vscode.commands.registerCommand("conjure.solve", () => {
        //     ConjureHelper.solveAndVisualise(false)
        //   })
        // )
        // context.subscriptions.push(
        //   vscode.commands.registerCommand("conjure.solveAndVis", () => {
        //     ConjureHelper.solveAndVisualise(true)
        //   })
        // )
        // context.subscriptions.push(
        //   vscode.commands.registerCommand("conjure.vis", () => {
        //     ConjureHelper.launchVisualiserWithPath()
        //   })
        // )
        // context.subscriptions.push(
        vscode.commands.registerCommand("conjure.configure", () => {
          // ConfigureHelper.launch()
          vscode.window.showInformationMessage(
            `http://localhost:${vscodeServePort}`
          )
          vscode.window.showInformationMessage(
            `Tree visualiser running at:      `
          )
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

export function deactivate() { }
