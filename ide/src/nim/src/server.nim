import asyncdispatch, jester, os, strutils, json, re, times, parseutils
import util/main
import util/jsonify
import util/types
import util/init
import util/response


router mainRouter:
    options re"(.*)":
        resp(Http200, [
            ("Access-Control-Allow-Origin", "*"),
            ("Access-Control-Allow-Headers", "content-type"),
            ("Access-Control-Allow-Method", "POST")
        ], "ok")

    get "/":
        resp readFile("/home/tom/conjure/ide/src/webview/ts/test.html")

    get re"/init/(.*)":
        let path = request.matches[0]

        var response: InitResponse

        # try:
        response = newInitResponse(path)
        # except:
        #     let e = getCurrentException()
        #     let msg = getCurrentExceptionMsg()

        #     echo msg

        #     resp(Http200, [("Access-Control-Allow-Origin", "*")], ${"error":msg})

        resp(Http200, [("Access-Control-Allow-Origin", "*")], $(%response))
        # resp(Http200, [("Content-Type","text/css")] , "foo")

    post "/simpleDomains":
        let json = parseJson($request.body)
        resp(Http200, [("Access-Control-Allow-Origin", "*")], $(
                %loadSimpleDomains(json["path"].getStr(), $json[
                "nodeId"].getInt(), true)))

    post "/loadNodes":
        let json = parseJson($request.body)
        resp(
            Http200,
            [("Access-Control-Allow-Origin", "*")],
            $(
                %loadNodes(
                    json["path"].getStr(),
                    $json["nodeId"].getInt(),
                    $json["depth"].getInt()))
            )

    post "/loadAncestors":
        let json = parseJson($request.body)
        echo json
        resp(Http200, [("Access-Control-Allow-Origin", "*")], $(%loadAncestors(
                json["path"].getStr(), $json["nodeId"].getInt())))

    post "/diff":
        let json = parseJson($request.body)
        resp(Http200, [("Access-Control-Allow-Origin", "*")], $(diffHandler(
                json["path1"].getStr(), $json["path2"].getStr(), json[
                "hash1"].getStr(), $json["hash2"].getStr())))


    # get "/longestBranchingVariable":
    #     resp(Http200, [("Access-Control-Allow-Origin", "*")], getLongestBranchingVarName())

    # get "/loadSet/@nodeId/@path":
    #     resp(Http200, [("Access-Control-Allow-Origin", "*")], $loadSetChild(@"nodeId",@"path"))


proc main() =

    var port: int
    if paramCount() == 0:
        port = 5000
    else:
        port = paramStr(1).parseInt()

    echo "NIM SERVER STARTING ON " & $port

    let settings = newSettings(port = Port(port))
    var jester = initJester(mainRouter, settings = settings)
    jester.serve()

when isMainModule:
    main()
