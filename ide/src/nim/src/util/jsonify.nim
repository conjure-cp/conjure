import json, tables, strutils, parseutils, sequtils, db_sqlite, intsets, algorithm
import types, process


proc setToTreeView*(s: Set): TreeViewNode =
    ## Converts a set to its representation as a tree view node
    var representation: string

    if (s of OccurrenceSet):
        representation = "Occurrence"
    if (s of DummySet):
        representation = "Dummy"
    if (s of MarkerSet):
        representation = "Marker"
    if (s of FlagSet):
        representation = "Flags"
    if (s of ExplicitSet):
        representation = "Explicit"

    let rep = TreeViewNode(name: "Representation", children: @[TreeViewNode(name: representation)])
    let cardinality = TreeViewNode(name: "Cardinality", children: @[
            TreeViewNode(name: s.getCardinality())])
        
    let included = TreeViewNode(name: "Included", children: @[TreeViewNode(  name: ($s.getPrettyIncluded()))])
    let notExcluded = TreeViewNode(name: "Not excluded", children: @[TreeViewNode(  name: ($s.getPrettyNotExcluded()))])

    let kids = TreeViewNode(name: "Children", children: @[])

    if (s.inner == nil):
        return (TreeViewNode(name: s.name, children: @[rep, cardinality, notExcluded, included]))

    return (TreeViewNode(name: s.name, children: @[rep, cardinality, kids]))


proc prettyDomainsToTreeView*(domains: seq[Variable]): TreeViewNode =
    ## Converts list of pretty domains to a tree view node
    let root = TreeViewNode(name: "Items")
    let variables = TreeViewNode(name: "Domain Variables")
    let expressions = TreeViewNode(name: "Expressions")
    let changedExpressions = TreeViewNode(name: "Changed Expressions")

    root.children = @[variables, expressions, changedExpressions]

    func compare(var1, var2: Variable): int =
        return cmp(var1.name, var2.name)
    
    var copy = domains
    copy.sort(compare)

    for d in copy:
        if d of Expression:
            expressions.children.add(TreeViewNode(name: d.name, children: @[TreeViewNode(name: d.rng)]))

        elif d of Set:
            let s = Set(d)
            let treeRep = (setToTreeView(s))
            variables.children.add(treeRep)

        else:
            variables.children.add(TreeViewNode(name: d.name, children: @[TreeViewNode(name: d.rng)]))

    return root

proc getCollapsedSetChildren*(s: Set): JsonNode =
    ## Return json list of collapsed child sets
    let json = %*{}
    if s.children.len() > 0:
        json["children"] = %*[]

        for kid in s.children:
            json["children"].add(%*{"name": kid.name, "_children": []})

    return json

proc expressionsToJson*(expressions: seq[Expression]): JsonNode =
    ## Converts a list of Expressions to their tree view representaion 
    var list: seq[TreeViewNode]
    for exp in expressions:
        list.add(TreeViewNode(name: exp.name, children: @[TreeViewNode(name: exp.rng)]))
    return %list

proc setToJson*(s: Set, nodeId: string, wantCollapsedChildren: bool): JsonNode =
    ## Returns the JSON representation of a set
    let json = %*{}
    json["name"] = %s.name
    json["Cardinality"] = %s.getCardinality()
    if (s.inner == nil):
        json["Included"] = %s.getPrettyIncluded()
        json["Not excluded"] = %s.getPrettyNotExcluded()
    else:
        if wantCollapsedChildren:
            json["Children"] = getCollapsedSetChildren(s)
    return json

proc getSetChanges*(newSet, oldSet: Set, isNested: bool = false): seq[string] =
    ## Return a list of strings representing the changed properties of a set
    let prefix = newSet.name

    if (newSet.getCardinality() != oldSet.getCardinality()):
        result.add(prefix & "Cardinality")

    if (newSet.getIncluded() != oldSet.getIncluded()):
        result.add(prefix & "Included")

    if (newSet.getNotExcluded() != oldSet.getNotExcluded()):
        result.add(prefix & "Not excluded")

    if isNested:
        if result.len() > 0:
            return @[prefix]
        else:
            return @[]

proc getPrettyChanges*(domainsAtnode, domainsAtPrev: seq[Variable]): (seq[string], seq[Expression]) =
    ## Returns a tuple 
    ## First element contains a list of variable names that have changed
    ## Second element contains a list of expressions that have changed
    var changedExpressions : seq[Expression]
    var changedVariableNames: seq[string]

    for i in countUp(0, min(domainsAtPrev.len(), domainsAtNode.len()) - 1):
        let newVar = domainsAtNode[i]
        let oldVar = domainsAtPrev[i]

        if (newVar of Set):
            let newSet = Set(newVar)
            let oldSet = Set(oldVar)
            changedVariableNames = changedVariableNames.concat(getSetChanges(newSet, oldSet))

            if (newSet.children.len() > 0 and newSet.children.len() == oldSet.children.len()):
                for i in countUp(0, newSet.children.len() - 1):
                    changedVariableNames = changedVariableNames.concat(getSetChanges(newSet.children[i], oldSet.children[i], true))

        elif newVar.rng != oldVar.rng:
            if (newVar of Expression):
                    changedExpressions.add(Expression(newVar))
            changedVariableNames.add(newVar.name)
    
    return (changedVariableNames, changedExpressions)
