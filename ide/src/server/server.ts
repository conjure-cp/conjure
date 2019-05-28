import * as vscode from 'vscode';
import * as express from "express";
import { Server, Path, GET, PathParam, QueryParam, Errors } from "typescript-rest";
import * as init from "./init";

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
        try{
            init.findFiles(path);
        }
        catch(e){
            vscode.window.showErrorMessage(e.message);
            return new Errors.BadRequestError("Invalid init path: " + e.message);
        }
        return "We got this path " + path;
    }
}

export function startServer() {
    let app: express.Application = express();
    Server.buildServices(app);

    app.listen(3000, function () {
        console.log('Rest Server listening on port 3000!');
    });
}
