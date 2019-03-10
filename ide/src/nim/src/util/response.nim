import main, types, json

type InitResponse* = ref object of RootObj
    prettyAtRoot*: TreeViewNode
    simpleAtRoot*: SimpleDomainResponse
    core*: Core
    

proc newInitResponse*(path: string): InitResponse =

    let core = init(path)
    let prettyAtRoot = getSkeleton()
    let simpleAtRoot = loadSimpleDomains("0")

    return InitResponse(prettyAtRoot: prettyAtRoot, simpleAtRoot: simpleAtRoot, core: core)

