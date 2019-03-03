import json, re, jester, times, strutils
import util/main
import util/jsonify
import util/types

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
        try:
            init(path)

        except EprimeParseException:
            resp HttpCode(503)
            echo "Failed to parse Eprime file"
        except MinionParseException:
            resp HttpCode(502)
            echo "Failed to parse Minion file"
        except :
            # resp HttpCode(501)
            echo("IOERROR!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")

        # benchmark "getPretty":

        let prettyAtRoot = getSkeleton()

        # benchmark "getSimple":

        let simpleAtRoot = %loadSimpleDomains("0")

        resp %*{"pretty" : prettyAtRoot, "simple": simpleAtRoot}
            
        # resp %*{"pretty": getSkeleton() "simple": loadSimpleDomains("0")}
        # resp "OK"

    get "/simpleDomains/@amount/@start/@nodeId":
        resp %loadSimpleDomains(@"nodeId")
        
    get "/prettyDomains/@nodeId/@paths?":
        resp loadPrettyDomains(@"nodeId", @"paths")

    get "/loadNodes/@amount/@start":
        resp %loadNodes(@"amount", @"start")

    get "/longestBranchingVariable":
        resp getLongestBranchingVarName()
        # resp 100

    get "/loadCore":
        # resp %proccessCore()
        resp %loadCore()

    get "/loadChildren/@id":
        resp %loadNodes("1", @"id")
        # resp %loadChildren(@"id")

    get "/loadSet/@nodeId/@path":
        resp loadSetChild(@"nodeId",@"path")