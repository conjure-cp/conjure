

proc parseAux(minionFilePath: string): Table[string, Expression] =
    var lookup = initTable[string, Expression]()
    let auxDef = re"aux\d* #(.*)"
    let minionFile = readFile(minionFilePath)
    let find = minionFile.findAll(auxDef)

    for a in find:
        let splitted = a.split("#")
        let name = splitted[0].strip()
        var rhs = splitted[1].replace(re"\(?Active-CSE: \d* occurrences of this expression or equivalent: ","")

        let nestedAux = re"aux\d*"

        for nested in rhs.findAll(nestedAux):
            if (lookup.hasKey(nested)):
                rhs = rhs.replace(nested, lookup[nested].name)

        lookup[name] = Expression(name: rhs)

    return lookup

proc parseEprime(eprimeFilePath: string): Table[string, Variable] =

    var varLookup = initTable[string, Variable]()
    var clean = ""

    for line in readFile(eprimeFilePath).split("Conjure's")[1].split("\n"):
        if len(line) == 0:
            continue
        clean &= line[1..^1]
    
    for key in parseJson(clean)["representations"].getElems():
        # echo key
        # try:

        # echo parseJson(clean).pretty()
        # echo key[0]

        if ( not key[0].haskey("Name")):
            continue

        let n = key[0]["Name"].getStr()

        if key[1].hasKey("DomainInt"):
            varLookup[n] = Variable(name: n)

        if key[1].hasKey("DomainSet"):
            
            let array = key[1]["DomainSet"].getElems()
            # echo array[^1].pretty()
            var bounds  : JsonNode

            bounds = array[^1]["DomainInt"].getElems()[0]

            if (bounds.hasKey("RangeBounded")):
                bounds = array[^1]["DomainInt"].getElems()[0]["RangeBounded"]
            else:
                bounds = array[^1]["DomainInt"].getElems()[1][0]["RangeBounded"]
            # echo bounds
            var l = bounds[0]["Constant"]["ConstantInt"].getInt(-1)
            var u = bounds[1]["Constant"]["ConstantInt"].getInt(-1)

            if (l == -1):
                l = bounds[0]["Constant"]["ConstantInt"][1].getInt(-1)
                u = bounds[1]["Constant"]["ConstantInt"][1].getInt(-1)

                if (l == -1):
                    echo "ERRORORORRORORORORRO"


            if array[0].hasKey("Set_ExplicitVarSizeWithDummy"):
                varLookup[n] = DummySet(name: n, lower: l, upper: u, dummyVal: u + 1) 

            elif array[0].hasKey("Set_Occurrence"):
                varLookup[n] = OccurrenceSet(name: n, lower: l, upper: u) 

            elif array[0].hasKey("Set_ExplicitVarSizeWithMarker"):
                varLookup[n] = MarkerSet(name: n, lower: l, upper: u) 

            elif array[0].hasKey("Set_ExplicitVarSizeWithFlags"):
                varLookup[n] = FlagSet(name: n, lower: l, upper: u) 
  
        # except:
        #         discard "Failed to parse Eprime"

    # echo varLookup
    return varLookup