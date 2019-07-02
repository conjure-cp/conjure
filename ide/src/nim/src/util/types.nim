import variable, setType, expression, json
export variable, setType, expression

# Constants
let maxIndex* = 9
let rootNodeId* = 0

# Exceptions
type
    InitException* = object of Exception
type
    CannotOpenEprimeException* = object of Exception
type
    CannotOpenMinionException* = object of Exception
type
    CannotOpenDatabaseException* = object of Exception
type
    EprimeParseException* = object of Exception
type
    MinionParseException* = object of Exception


# Responses
type PrettyDomainResponse* = ref object of RootObj
    vars*: JsonNode
    changed*: seq[string]
    changedExpressions*: JsonNode

type SimpleDomainResponse* = ref object of RootObj
    changedNames*: seq[string]
    vars*: seq[Variable]

type Node* = ref object of RootObj
    id*: int
    parentId*: int
    label*: string
    prettyLabel*: string
    childCount*: int
    isSolution*: bool
    isLeftChild*: bool
    descCount*: string

type Core* = ref object of RootObj
    nodes*: seq[Node]
    solAncestorIds*: seq[int]

type TreeViewNode* = ref object of RootObj
    name*: string
    children*: seq[TreeViewNode]

