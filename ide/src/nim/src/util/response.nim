import main, types, json

type InitResponse* = ref object of RootObj
    ## Type representing the response to a request at /init
    core*: Core
    info*: string
    

proc newInitResponse*(path: string): InitResponse =
    ## constructor
    let (core, info) = init(path)
    # let prettyAtRoot = getSkeleton()
    # let simpleAtRoot = loadSimpleDomains("0", true)

    return InitResponse(core: core, info: info)

