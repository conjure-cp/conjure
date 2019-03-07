import json, re, jester, times, strutils, parseutils
import util/main
import util/jsonify
import util/types
import util/init

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

        var core : JsonNode

        try:
            core = init(path)

        except:
            # let e = getCurrentException()
            let msg = getCurrentExceptionMsg()
            resp %*{"error":msg}
            # raise

        let prettyAtRoot = getSkeleton()

        let simpleAtRoot = %loadSimpleDomains("0")

        # let core = %getCore()

        resp %*{"pretty" : prettyAtRoot, "simple": simpleAtRoot, "core": core}

    get "/simpleDomains/@nodeId/@wantExpressions":

        resp %loadSimpleDomains(@"nodeId", parseBool(@"wantExpressions"))
        
    get "/prettyDomains/@nodeId/@wantExpressions/@paths?":
        resp loadPrettyDomains(@"nodeId", @"paths", parseBool(@"wantExpressions"))
        # resp loadPrettyDomains(@"nodeId", @"paths", true)

    get "/loadNodes/@start":
        resp %loadNodes(@"start")

    # get "/longestBranchingVariable":
        # resp getLongestBranchingVarName()
        # resp 100

    # get "/loadCore":
    #     # resp %proccessCore()
    #     resp %loadCore()

    get "/loadChildren/@id":
        resp %loadNodes(@"id")

    get "/loadSet/@nodeId/@path":
        resp loadSetChild(@"nodeId",@"path")