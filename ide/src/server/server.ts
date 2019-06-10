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

import ConfigHelper from '../configHelper';
import WebviewHelper from '../webviewHelper';

const collator = new Intl.Collator(undefined, { numeric: true })
const hasher = apiConstructor({ sort: true, coerce: true }).hash

interface Response {
    models: string[]
    params: string[]
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
    async startSearch(list: any) {

        if (list.length > 1 && JSON.stringify(list[0]) === JSON.stringify(list[1])) {
            vscode.window.showErrorMessage("Configs are the same! aborting..")
            return
        }

        const { needToGenerate, loadFromCache } = await ConfigHelper.separateJobs(list)

        return vscode.window.withProgress({
            cancellable: true,
            location: vscode.ProgressLocation.Notification,
            title: 'Solving '

        }, (progress, token) => {
            return ConfigHelper.makePromise(needToGenerate, progress, token)
        }).then(() => {
            console.log("Everything done")
            return {
                message: "hello!",
                content: WebviewHelper.getWebContent()
            }
        })
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
