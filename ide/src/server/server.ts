import fs = require('fs')
import rimraf = require("rimraf")
import * as path from 'path'
import * as vscode from 'vscode';
import * as express from "express";
import { Server, Path, GET, POST, PathParam, QueryParam, Errors } from "typescript-rest";
import * as init from "./init";
import * as cors from 'cors'
import apiConstructor = require('node-object-hash')
import { spawn, ChildProcess } from 'child_process'

const collator = new Intl.Collator(undefined, { numeric: true })
const hasher = apiConstructor({ sort: true, coerce: true }).hash

interface File {
    shortPath: string
    fullPath: string
}

interface Response {
    models: string[]
    params: string[]
}

function configToArgs(config: any, hash: string): string[] {


    const fullPathToModel = path.join(vscode.workspace.rootPath!, config.essenceFile)
    const fullPathToParam = path.join(vscode.workspace.rootPath!, config.paramFile)

    let conjureOptions = ["solve", fullPathToModel, fullPathToParam, "-o", hash]

    if ("conjureTime" in config) {
        conjureOptions.push(`--limit-time=${config.conjureTime}`)
    }

    if ("strategy" in config) {
        conjureOptions.push("-a")
        conjureOptions.push(config.strategy)
    }

    let savileRowOptions = [
        "--savilerow-options",
        "\"",
    ]

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

    savileRowOptions.push("\"")

    let minionOptions = ["--solver-options", "\"-dumptreesql", hash + "/out.db"]

    if ("minionSwitches" in config) {
        minionOptions.concat(config.minionSwitches)
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

    minionOptions.push("\"")

    conjureOptions = conjureOptions.concat(savileRowOptions).concat(minionOptions)

    console.log("conjure " + conjureOptions.join(" "))

    return conjureOptions
}

function getProgressMessage(doneCount: number, jobCount: number, proc: ChildProcess, pid2JobId: any): string {
    return `[${doneCount + 1}/${jobCount}] - Config` + pid2JobId[proc.pid] + ' - '
}

@Path("/config")
class ConfigService {

    toRel(uri: vscode.Uri): string {
        return vscode.workspace.asRelativePath(uri.path);
    }

    @Path("/files")
    @GET
    async getFiles(): Promise<Response> {
        let models = await vscode.workspace.findFiles("**/*.essence")
        let params = await vscode.workspace.findFiles("**/*.param")

        return {
            models: models.map(uri => this.toRel(uri)).sort(collator.compare),
            params: params.map(uri => this.toRel(uri)).sort(collator.compare)
        }
    }
    @Path("/solve")
    @POST
    startSearch(list: any) {
        console.log(list)

        let config = list[0]
        const hash = hasher(config)
        let args = configToArgs(config, hash)

        vscode.window.withProgress({
            cancellable: true,
            location: vscode.ProgressLocation.Notification,
            title: 'Solving '

        }, (progress, token) => {

            var p = new Promise((resolve, _reject) => {

                let doneCount = 0
                let procs: ChildProcess[] = []
                let pid2JobId: any = {}

                for (let i = 0; i < 1; i++) {

                    const proc = spawn("conjure", args, {
                        shell: true,
                        cwd: vscode.workspace.rootPath,
                        detached: true
                    })

                    pid2JobId[proc.pid] = i + 1

                    progress.report({
                        message:
                            `${getProgressMessage(doneCount, 1, proc, pid2JobId)}
                             Generating models..`,
                        increment: 0
                    })

                    let errorMessage = ""

                    proc.stdout.on('data', (data) => {
                        data = data.toString()
                        console.log(data.toString())

                        if (data.includes("Generating")) {

                            progress.report({
                                message:
                                    `${getProgressMessage(doneCount, 1, proc, pid2JobId)}
                                Running savilerow..`,
                                increment: 0
                            })
                        }
                        if (data.includes("domain")) {
                            progress.report({
                                message:
                                    `${getProgressMessage(doneCount, 1, proc, pid2JobId)}
                                Domain filtering..`,
                                increment: 0
                            })
                        }
                        if (data.includes("solver")) {
                            progress.report({
                                message:
                                    `${getProgressMessage(doneCount, 1, proc, pid2JobId)}
                                Running Minion..`,
                                increment: 0
                            })
                        }
                    })

                    proc.stderr.on('data', (data) => {
                        errorMessage += `${data}`
                    })

                    proc.on('close', (code) => {

                        doneCount++

                            progress.report({
                                message:
                                    `${getProgressMessage(doneCount, 1, proc, pid2JobId)}
                                Running Minion..`,
                                increment: 0
                            })

                        fs.writeFileSync(path.join(vscode.workspace.rootPath!, hash, "vscode.extensionCache"), "blank")

                        console.log(`child process exited with code ${code}`)
                        console.error(errorMessage)
                        if (errorMessage === "") {
                            // let command = "conjure " + job.args.join(" ")
                            // WebviewHelper.launch(path.join(vscode.workspace.rootPath!, job.hash), command)
                            console.log("Success")
                        }
                        else {
                            vscode.window.showErrorMessage("Config " + pid2JobId[proc.pid] + " | " + errorMessage)
                            rimraf.sync(path.join(vscode.workspace.rootPath!, hash))
                        }

                        if (doneCount === 1) {
                            resolve()
                        }

                    })

                    proc.on('error', (err) => {
                        console.log('Failed to start subprocess.')
                        console.error(err)
                        vscode.window.showErrorMessage("Failed to start conjure ;_;")
                    })

                    token.onCancellationRequested(() => {
                        procs.map((proc: ChildProcess) => process.kill(-proc.pid))
                        errorMessage = "Search Cancelled!"
                        // jobs.map((j) => rimraf.sync(path.join(vscode.workspace.rootPath!, j.hash)))
                        // cached.map((c) => rimraf.sync(path.join(vscode.workspace.rootPath!, c.hash)))
                    })
                }

                // cached.forEach(search => {
                //     let command = "conjure " + search.args.join(" ")
                //     WebviewHelper.launch(path.join(vscode.workspace.rootPath!, search.hash), command)
                // })

                // if (jobs.length === 0) {
                //     resolve()
                // }
            }).then(() => {
                vscode.window.showInformationMessage("Done")
            })
            return p
        }) // vscode.window.withProgress
    }



}

@Path("/hello")
class HelloService {
    @Path(":name")
    @GET
    sayHello(@PathParam('name') name: string): string {
        return "Hello " + name;
    }
}

@Path("/init")
class Init {
    @GET
    init(@QueryParam("path") path: string) {
        try {
            init.findFiles(path);
        }
        catch (e) {
            vscode.window.showErrorMessage(e.message);
            return new Errors.BadRequestError("Invalid init path: " + e.message);
        }
        return "We got this path " + path;
    }
}

export function startServer() {
    let app: express.Application = express()
    app.use(cors())
    Server.buildServices(app)

    app.listen(4000, function () {
        console.log('Rest Server listening on port 4000!');
    });
}
