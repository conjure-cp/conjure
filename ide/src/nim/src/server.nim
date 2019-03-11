import json, re, jester, times, strutils, parseutils
import util/main
import util/jsonify
import util/types
import util/init
import util/response

template benchmark(benchmarkName: string, code: untyped) =
  block:
    let t0 = epochTime()
    code
    let elapsed = epochTime() - t0
    let elapsedStr = elapsed.formatFloat(format = ffDecimal, precision = 3)
    echo "CPU Time [", benchmarkName, "] ", elapsedStr, "s"

routes:
    get re"/init/(.*)":
        let path = request.matches[0]

        var response: InitResponse

        # try:
        response = newInitResponse(path)
        # except:
        #     let e = getCurrentException()
        #     let msg = getCurrentExceptionMsg()
        #     resp %*{"error":msg}
        resp %response 

    get "/simpleDomains/@nodeId/@wantExpressions":
        resp %loadSimpleDomains(@"nodeId", parseBool(@"wantExpressions"))
        
    get "/prettyDomains/@nodeId/@wantExpressions/@paths?":
        resp %loadPrettyDomains(@"nodeId", @"paths", parseBool(@"wantExpressions"))

    get "/loadNodes/@start":
        resp %loadNodes(@"start")

    get "/longestBranchingVariable":
        resp getLongestBranchingVarName()

    # get "/loadCore":
    #     # resp %proccessCore()
    #     resp %loadCore()

    get "/loadChildren/@id":
        resp %loadNodes(@"id")

    get "/loadSet/@nodeId/@path":
        resp loadSetChild(@"nodeId",@"path")