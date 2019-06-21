import main, types, json

type InitResponse* = ref object of RootObj
    ## Type representing the response to a request at /init
    prettyAtRoot*: TreeViewNode
    simpleAtRoot*: SimpleDomainResponse
    core*: Core
    info*: string
    

proc newInitResponse*(path: string): InitResponse =
    ## constructor
    let (core, info) = init(path)
    let prettyAtRoot = getSkeleton()
    let simpleAtRoot = loadSimpleDomains("0", true)

    return InitResponse(prettyAtRoot: prettyAtRoot, simpleAtRoot: simpleAtRoot, core: core, info: info)

