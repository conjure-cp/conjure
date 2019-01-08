import re, strutils, os, tables, json, db_sqlite, parseutils
include types, parser
export re, strutils, os, tables, json, db_sqlite, parseutils
# import "parseEprime.nim", "parseAux.nim"
var eprimeLookup : Table[string, Variable]
var auxLookup : Table[string, Expression]

proc initParser(minionFilePath: string, eprimeFilePath: string) =
    eprimeLookup = parseEprime(eprimeFilePath)
    auxLookup = parseAux(minionFilePath)

proc getSimpleDomainsOfNode(db: DbConn, amount: string, start: string, nodeId: string): seq[Variable] =

    var domains : seq[Variable]

    for domain in db.fastRows(sql"select name, lower, upper from domain where nodeId = ? and domainId >= ? limit ?",nodeId, start, amount):

        # echo domain[0]
        # echo auxLookup

        if (auxLookup.hasKey(domain[0])):
            let d = auxLookup[domain[0]]
            d.rng = getPrettyRange(domain[1], domain[2])
            domains.add(d)
        else:
            let v = Variable(name: domain[0])
            v.rng = getPrettyRange(domain[1], domain[2])
            domains.add(v)

    return domains

proc getPrettyDomainsOfNode(db: DbConn, nodeId: string) : seq[Variable] =
    # echo auxLookup
    var domains : seq[Variable]

    var tableCopy : Table[string, Variable]
    tableCopy.deepCopy(eprimeLookup)

    for domain in db.fastRows(sql"select name, lower, upper from domain where nodeId = ?", nodeId):
        # echo domain

        if (auxLookup.hasKey(domain[0])):
            let d = auxLookup[domain[0]]
            d.rng = getPrettyRange(domain[1], domain[2])
            domains.add(d)
        
        elif (tableCopy.hasKey(domain[0])):
            let v = tableCopy[domain[0]]
            v.rng = getPrettyRange(domain[1], domain[2])
            domains.add(v)

        else:
            let splitted = domain[0].split("_")
            let setName = splitted[0]
            let lower = domain[1]
            let upper = domain[2]

            if tableCopy.hasKey(setName):

                let s = tableCopy[setName]
                # echo splitted
                try:
                    let num = parseInt(splitted[^1])

                    if s of DummySet:

                        let dummySet = cast[DummySet](s)

                        if (lower != $dummySet.dummyVal):
                            dummySet.included.add(num)
                        else:
                            dummySet.excluded.add(num)


                        # if (not (dummySet in domains)):
                        #     domains.add(dummySet)

                    elif s of OccurrenceSet:
                        let oSet = cast[OccurrenceSet](s)

                        if (lower == upper):
                            if (lower == "1"):
                                oSet.included.add(num)
                            else:
                                oSet.excluded.add(num)
                            
                            # echo "HERE"

                    elif s of MarkerSet:
                        let mSet = cast[MarkerSet](s)

                        if (lower == upper):
                            var l : int
                            discard parseInt(lower, l)
                            mSet.included.add(l)

                    elif s of FlagSet:
                        let fSet = cast[FlagSet](s)

                        if (domain[0].contains("_ExplicitVarSizeWithFlags_Flags_")):
                            if (lower == upper):
                                if (lower == "1"):
                                    fSet.list.add("")
                                else:
                                    discard fSet.list.pop()
                        else:
                            if (lower == upper):
                                var l : int
                                discard parseInt(lower, l)
                                fSet.included.add(l)


                except ValueError:
                    
                    let mSet = cast[MarkerSet](s)
                    mSet.cardinality = getPrettyRange(lower, upper)
                    

                if (not (s in domains)):
                    domains.add(s)

    return domains


proc domainsToJson(domains: seq[Variable]): JsonNode =

    let root = TreeViewNode(name: "Items")
    let variables = TreeViewNode(name: "Variables")
    let expressions = TreeViewNode(name: "Expressions")
    let sets = TreeViewNode(name: "Sets")

    root.children = @[variables, sets, expressions]

    for d in domains:

        if d of Expression:
            expressions.children.add(TreeViewNode(name: d.name, children: @[TreeViewNode(name: d.rng)]))

        elif d of Set:

            let s = cast[Set](d)
            var setType : string

            if (s of OccurrenceSet):
                setType = "Occurrence"
            if (s of DummySet):
                setType = "Dummy"
            if (s of MarkerSet):
                setType = "Marker"
            if (s of FlagSet):
                setType = "Flags"

            let t = TreeViewNode(name: "Type", children: @[TreeViewNode(name: setType)])
            let cardinality = TreeViewNode(name: "Cardinality", children: @[TreeViewNode(name: s.getCardinality())])
            let included = TreeViewNode(name: "Included", children: @[TreeViewNode(name: ($s.included)[2..^2])])
            let excluded = TreeViewNode(name: "Excluded", children: @[TreeViewNode(name: ($s.excluded)[2..^2])])
            sets.children.add(TreeViewNode(name: s.name, children: @[t, cardinality, included, excluded]))

        else:
            variables.children.add(TreeViewNode(name: d.name, children: @[TreeViewNode(name: d.rng)]))
        
    # let list = @[root]
    # let json = (%list).pretty()
    return %root