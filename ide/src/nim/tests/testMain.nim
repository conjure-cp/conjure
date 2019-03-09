import unittest, json, constants, strutils
import util/types
import util/types
import util/main
import util/jsonify

suite "init":
    test "initValidPath":
        let validPath = testDataPath & "/sets/dummy"
        discard init(validPath)

suite "loadNodes":
    let validPath = testDataPath & "/sets/recursive/markerMarkerMarker"
    discard init(validPath)


suite "loadSimpleDomains":
    let validPath = testDataPath & "/sets/recursive/markerMarkerMarker"
    discard init(validPath)
    test "changed":
        var response = loadSimpleDomains("0")
        # echo %response

        check(response.changedNames.len() == 0)

        response = loadSimpleDomains("2")
        check(response.changedNames == @["y"])

        response = loadSimpleDomains("3")
        check(response.changedNames == @["z"])

        response = loadSimpleDomains("4")
        check(response.changedNames == @[
                "s_ExplicitVarSizeWithMarkerR5R5_Marker"])

    test "expressions":
        check(loadSimpleDomains("1", true).vars.len() > loadSimpleDomains("1", false).vars.len())

suite "loadPrettyDomains":
    let validPath = testDataPath & "/sets/recursive/markerMarkerMarker"
    discard init(validPath)

    test "prettyDomainUpdateRoot":
        let nodeId = "0"

        let pretty0 = loadPrettyDomains(nodeId, "")
        let vars = pretty0.vars

        let y = %*{"name": "y", "rng": "int(1..9)"}
        check(vars[0] == y)

        check(vars[1] == setToJson(getSet(nodeId), nodeId, true))

        let z = %* {"name": "z", "rng": "int(1..9)"}
        check(vars[2] == z)
        
        let x = %* {"name": "x", "rng": "int(1..9)"}
        check(vars[3] == x)

        check(pretty0.changed.len() == 0)
        check(pretty0.changedExpressions.len() == 0)

    test "prettyDomainUpdateNode1":

        let nodeId = "1"

        let pretty1 = loadPrettyDomains(nodeId, "")
        let vars = pretty1.vars

        let y = %*{"name": "y", "rng": "int(1..9)"}
        check(vars[0] == y)

        check(vars[1] == setToJson(getSet(nodeId), nodeId, true))

        let z = %* {"name": "z", "rng": "int(1..9)"}
        check(vars[2] == z)
        
        let x = %* {"name": "x", "rng": "int(1)"}
        check(vars[3] == x)

        check(pretty1.changed == @["x"])
        check(pretty1.changedExpressions.len() == 0)

    test "prettyDomainUpdateExpandedChild":

        let nodeId = "2"

        let pretty1 = loadPrettyDomains(nodeId, "s.s-1")
        let vars = pretty1.vars

        let y = %*{"name": "y", "rng": "int(1)"}
        check(vars[0] == y)

        check(vars[1] == setToJson(getSet(nodeId), nodeId, true))

        let z = %* {"name": "z", "rng": "int(1..9)"}
        check(vars[2] == z)
        
        let x = %* {"name": "x", "rng": "int(1)"}
        check(vars[3] == x)

        check(vars[4] == setToJson(getSet(nodeId).children[0], nodeId, true))

        check(pretty1.changed == @["y"])
        check(pretty1.changedExpressions.len() == 0)

    test "collapsedSetsAppearInChangedList":
        let nodeId = "5"
        let pretty5 = loadPrettyDomains(nodeId, "s.s-1")
        check("s-1" in pretty5.changed)


    test "getExpandedSetChildFailed":
        let nodeId = "2"
        discard loadPrettyDomains(nodeId, "")
        check(getExpandedSetChild(nodeId, "s.3.ASDASD") == nil)

    test "getExpandedSetChildSuccess":
        let nodeId = "2"
        discard loadPrettyDomains(nodeId, "")
        check(getExpandedSetChild(nodeId, "s.s-1") != nil)

    test "loadSetChild":
        let expected = """{
        "structure": {
          "name": "s-1",
          "children": [
            {
              "name": "Type",
              "children": [
                {
                  "name": "Marker",
                  "children": []
                }
              ]
            },
            {
              "name": "Cardinality",
              "children": [
                {
                  "name": "int(1..4)",
                  "children": []
                }
              ]
            },
            {
              "name": "Children",
              "children": []
            }
          ]
        },
        "update": {
          "name": "s-1",
          "Cardinality": "int(1..4)",
          "Children": {
            "children": [
              {
                "name": "s-1-1",
                "_children": []
              }
            ]
          }
        },
        "path": "s.s-1"
        }"""

        check(loadSetChild("2", "s.s-1") == parseJson(expected))

suite "dontCrash":
    let validPath = testDataPath & "/sets/recursive/markerMarkerFlags"
    discard init(validPath)

    test "bug1":
        let pretty1 = loadPrettyDomains("1", "")

    test "bug2":
        let pretty1 = loadPrettyDomains("1", "s.s-1:s.s-1.s-1-1:s.s-2:s.s-2.s-2-1")

