import json, re, jester, times, strutils, parseutils
import util/main
import util/jsonify
import util/types
import util/init
import util/response

routes:
    get re"/init/(.*)":
        let path = request.matches[0]

        var response: InitResponse

        try:
            response = newInitResponse(path)
        except:
            let e = getCurrentException()
            let msg = getCurrentExceptionMsg()
            resp %*{"error":msg}

        resp %response 

    get "/simpleDomains/@nodeId/@wantExpressions":
        resp %loadSimpleDomains(@"nodeId", parseBool(@"wantExpressions"))
        
    get "/prettyDomains/@nodeId/@wantExpressions/@paths?":
        resp %loadPrettyDomains(@"nodeId", @"paths", parseBool(@"wantExpressions"))

    get "/loadNodes/@nodeId":
        resp %loadNodes(@"nodeId")

    get "/longestBranchingVariable":
        resp getLongestBranchingVarName()

    get "/loadSet/@nodeId/@path" :
        resp loadSetChild(@"nodeId",@"path")