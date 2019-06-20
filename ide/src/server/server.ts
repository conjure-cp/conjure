import * as path from "path";
import * as vscode from "vscode";
import * as express from "express";
import fs = require("fs");

import {
  Server,
  Path,
  GET,
  POST,
  PathParam,
  QueryParam,
  Errors
} from "typescript-rest";

import * as init from "./init";
import * as cors from "cors";
const fetch = require("node-fetch");

import ConfigHelper from "../configHelper";
import { Cache } from "../configHelper";

const collator = new Intl.Collator(undefined, { numeric: true });

interface Response {
  models: string[];
  params: string[];
}

@Path("/config")
class ConfigService {
  toRel(uri: vscode.Uri): string {
    return vscode.workspace.asRelativePath(uri.path);
  }

  @Path("/caches")
  @GET
  async getCaches(): Promise<Cache[]> {
    let cachedFiles = await vscode.workspace.findFiles(
      `**/${ConfigHelper.cacheFileName}`
    );

    return cachedFiles.map(uri => {
      return {
        name: path.basename(path.dirname(uri.path)),
        config: JSON.parse(fs.readFileSync(uri.path).toString()).config
      };

      // const json: any = JSON.parse(fs.readFileSync(uri.path).toString());
      // console.log(json);
      // return json;

      // return {
      //   pathToFolder: path.dirname(uri.path),
      //   timeStamp: json.timeStamp,
      //   config: json.args
      // };
    });
  }

  @Path("/files")
  @GET
  async getFiles(): Promise<Response> {
    let models = await vscode.workspace.findFiles("**/*.essence");
    let params = await vscode.workspace.findFiles("**/*.param");

    return {
      models: models.map(uri => this.toRel(uri)).sort(collator.compare),
      params: params.map(uri => this.toRel(uri)).sort(collator.compare)
    };
  }

  @Path("/solve")
  @POST
  async startSearch(list: Cache[]) {
    if (
      list.length > 1 &&
      JSON.stringify(list[0]) === JSON.stringify(list[1])
    ) {
      vscode.window.showErrorMessage("Configs are the same! aborting..");
      return;
    }

    const { needToGenerate, loadFromCache } = await ConfigHelper.separateJobs(
      list
    );

    return vscode.window
      .withProgress(
        {
          cancellable: true,
          location: vscode.ProgressLocation.Notification,
          title: "Solving "
        },
        (progress, token) => {
          return ConfigHelper.makePromise(needToGenerate, progress, token);
        }
      )
      .then(async () => {
        const trees = needToGenerate.concat(loadFromCache);

        const fullPath = path.join(ConfigHelper.cacheFolderPath, trees[0].name);

        return await fetch(`http://localhost:5000/init/${fullPath}`).then(
          (response: any) =>
            response.json().then((json: any) => {
              json["core"]["id"] = trees[0].hash;
              return json;
            })
        );
      });
  }
}

@Path("/hello")
class HelloService {
  @Path(":name")
  @GET
  sayHello(@PathParam("name") name: string): string {
    return "Hello " + name;
  }
}

@Path("/init")
class Init {
  @GET
  init(@QueryParam("path") path: string) {
    try {
      init.findFiles(path);
    } catch (e) {
      vscode.window.showErrorMessage(e.message);
      return new Errors.BadRequestError("Invalid init path: " + e.message);
    }
    return "We got this path " + path;
  }
}

export function startServer() {
  let app: express.Application = express();
  app.use(cors());
  Server.buildServices(app);

  app.listen(4000, function() {
    console.log("Rest Server listening on port 4000!");
  });
}
