import jester
import util/main

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

        resp "OK"

    get "/simpleDomains/@amount/@start/@nodeId":
        resp loadSimpleDomains(@"amount", @"start", @"nodeId")
        
    get "/prettyDomains/@nodeId/@paths?":
        resp loadPrettyDomains(@"nodeId", @"paths")

    get "/loadNodes/@amount/@start":
        resp loadNodes(@"amount", @"start")

    get "/longestBranchingVariable":
        resp getLongestBranchingVarName()

    # get "/allNodes":
    #     resp loadAllNodes()

    get "/loadCore":
        resp loadCore()

    get "/loadChildren/@id":
        resp loadChildren(@"id")

    get "/loadSet/@nodeId/@path":
        resp loadSetChild(@"nodeId",@"path")

    # get "/loadChildSets/@id/@setName":
    #     resp loadChildSets(@"setName", @"id")