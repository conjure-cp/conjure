import jester
import util/main

routes:

    get re"/init/(.*)":
        let path = request.matches[0]
        init(path)
        resp "OK"

    get "/simpleDomains/@amount/@start/@nodeId":
        resp loadSimpleDomains(@"amount", @"start", @"nodeId")
        
    get "/prettyDomains/@amount/@start/@nodeId":
        resp loadPrettyDomains(@"amount", @"start", @"nodeId")

    get "/loadNodes/@amount/@start":
        resp loadNodes(@"amount", @"start")

    get "/correctPath":
        resp getCorrectPath()

    get "/longestBranchingVariable":
        resp getLongestBranchingVarName()
