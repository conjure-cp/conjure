import * as path from "path"
import * as vscode from "vscode"
import * as express from "express"
import fs = require("fs")
import { sortBy } from "lodash"

import {
  Server,
  Path,
  GET,
  POST,
  PathParam,
  QueryParam,
  Errors
} from "typescript-rest"

import * as init from "./init"
import * as cors from "cors"
const fetch = require("node-fetch")

import ConfigHelper, { VarRepresentation, RepMap } from "../configHelper"
import { Cache } from "../configHelper"
import { execSync } from "child_process"

const collator = new Intl.Collator(undefined, { numeric: true })

interface FilesResponse {
  models: string[]
  params: string[]
  representations: RepMap
}

let nimServerPort: number
let thisServerPort: number
let context: vscode.ExtensionContext

@Path("/")
class HTMlService {
  @Path("/")
  @GET
  getPage(): string {
    let html =
      fs
        .readFileSync(
          path.join(context.extensionPath, "src/config/configPage.html")
        )
        .toString() +
      `
      <div id="port" vscodeServerPort="${thisServerPort}"></div>
    <script>${fs
      .readFileSync(path.join(context.extensionPath, "dist/configBundle.js"))
      .toString()}</script>
  </body>
</html>
      `

    console.log(html)
    return html
  }
}

@Path("/config")
class ConfigService {
  toRel(uri: vscode.Uri): string {
    return vscode.workspace.asRelativePath(uri.path)
  }

  @Path("/caches")
  @GET
  async getCaches(): Promise<Cache[]> {
    let cachedFiles = await vscode.workspace.findFiles(
      `**/${ConfigHelper.cacheFileName}`
    )

    return sortBy(
      cachedFiles.map(uri => {
        return {
          name: path.basename(path.dirname(uri.path)),
          config: JSON.parse(fs.readFileSync(uri.path).toString()).config
        }
      }),
      ["name"]
    ).reverse()
  }

  @Path("/files")
  @GET
  async getFiles(): Promise<FilesResponse> {
    let models = await vscode.workspace.findFiles("**/*.essence")
    let params = await vscode.workspace.findFiles("**/*.param")

    let reps = models.map(model => {
      return {
        name: this.toRel(model),
        representations: JSON.parse(
          execSync(
            `conjure ide ${model.path} --dump-representations`
          ).toString()
        )
      }
    })

    let map: any = {}

    reps.forEach(r => {
      map[r.name] = r.representations
    })

    return {
      models: models.map(uri => this.toRel(uri)).sort(collator.compare),
      params: params.map(uri => this.toRel(uri)).sort(collator.compare),
      representations: map
    }
  }

  @Path("/solve")
  @POST
  async startSearch(list: Cache[]) {
    console.log("SOLLLLLLLLLLLLLLLLLLLLVE REQUEST")

    if (
      list.length > 1 &&
      JSON.stringify(list[0]) === JSON.stringify(list[1])
    ) {
      vscode.window.showErrorMessage("Configs are the same! aborting..")
      return
    }

    const { needToGenerate, loadFromCache } = await ConfigHelper.separateJobs(
      list
    )

    return vscode.window
      .withProgress(
        {
          cancellable: true,
          location: vscode.ProgressLocation.Notification,
          title: "Solving "
        },
        (progress, token) => {
          return ConfigHelper.makePromise(needToGenerate, progress, token)
        }
      )
      .then(() => {
        return vscode.window.withProgress(
          {
            cancellable: false,
            location: vscode.ProgressLocation.Notification,
            title: "Processing Tree"
          },
          async () => {
            const trees = needToGenerate.concat(loadFromCache)

            const fullPath = path.join(
              ConfigHelper.cacheFolderPath,
              trees[0].name
            )

            return await fetch(
              `http://localhost:${nimServerPort}/init/${fullPath}`
            ).then((response: any) =>
              response.json().then((json: any) => {
                json["core"]["id"] = trees[0].hash
                json["path"] = fullPath
                json["nimServerPort"] = nimServerPort
                json["vscodeServerPort"] = thisServerPort
                return json
              })
            )
          }
        )
      })
  }
}

@Path("/hello")
class HelloService {
  @Path(":name")
  @GET
  sayHello(@PathParam("name") name: string): string {
    return "Hello " + name
  }
}

@Path("/init")
class Init {
  @GET
  init(@QueryParam("path") path: string) {
    try {
      init.findFiles(path)
    } catch (e) {
      vscode.window.showErrorMessage(e.message)
      return new Errors.BadRequestError("Invalid init path: " + e.message)
    }
    return "We got this path " + path
  }
}

export function startServer(
  nsPort: number,
  vscodeServerPort: number,
  ctx: vscode.ExtensionContext
) {
  nimServerPort = nsPort
  thisServerPort = vscodeServerPort
  context = ctx

  let app: express.Application = express()
  app.use(cors())
  Server.buildServices(app)

  app.listen(vscodeServerPort, function() {
    console.log(`Rest Server listening on port ${vscodeServerPort}!`)
  })
}
