import unittest
import util/types
include util/main

suite "init":
    test "initValidPath":
        let validPath = "../test/testData/sets/dummy"
        init(validPath)

    test "initNoDBFile":
        let badPath = "../test/testData/extension/noDBFile"
        expect(CannotOpenDatabaseException):
            init(badPath)

    test "initNoEprimeFile":
        let badPath = "../test/testData/extension/noEprimeFile"
        expect(IOError):
            init(badPath)
    
    test "initNoMinionFile":
        let badPath = "../test/testData/extension/noMinionFile"
        expect(IOError):
            init(badPath)

suite "loadNodes":
    let validPath = "../test/testData/sets/recursive/markerMarkerMarker"
    init(validPath)

    test "fromStart":
        let nodes = loadNodes("3", "-1")
        check(nodes.len() == 3)
        check(nodes[0].nodeId == 0)
        check(nodes[0].parentId == -1)
        check(nodes[0].label == "")

        check(nodes[1].nodeId == 1)
        check(nodes[1].parentId == 0)
        check(nodes[1].label == "x = 1")

        check(nodes[2].nodeId == 2)
        check(nodes[2].parentId == 1)
        check(nodes[2].label == "y = 1")
        

    test "fromTwo":
        let nodes = loadNodes("3", "2")
        check(nodes.len() == 3)
        check(nodes[0].nodeId == 3)
        check(nodes[1].nodeId == 4)
        check(nodes[2].nodeId == 5)

suite "loadChildren":
    let validPath = "../test/testData/sets/recursive/markerMarkerMarker"
    init(validPath)

    test "oneChild":
        let response = loadChildren("1")
        check(response.nodeId == 1)
        check(response.children == @[2])

suite "loadCore":
    let validPath = "../test/testData/sets/recursive/markerMarkerMarker"
    init(validPath)

    test "1":
        let core = loadCore()
        check(core.len() == 10)

        for i in countUp(1, 7):
            check(core[i].nodeId == i)
            check(core[i].children[0] == i+1)

        check(core[1].label == "x = 1")
        check(core[2].label == "y = 1")
        check(core[3].label == "z = 1")

suite "getLabel":
    test "simple":
        check(getLabel("x", "0", "0") == "x != 0")
        check(getLabel("y", "4", "1") == "y = 4")

suite "loadSimpleDomains":
    let validPath = "../test/testData/sets/recursive/markerMarkerMarker"
    init(validPath)
    test "changed":
        var response = loadSimpleDomains("0")
        check(response.changedNames.len() == 0)

        response = loadSimpleDomains("2")
        check(response.changedNames == @["y"])

        response = loadSimpleDomains("3")
        check(response.changedNames == @["z"])

        response = loadSimpleDomains("4")
        check(response.changedNames == @["s_ExplicitVarSizeWithMarkerR5R5_Marker"])

    test "expressions":
        check(loadSimpleDomains("1", true).vars.len() > loadSimpleDomains("1", false).vars.len())


suite "loadPrettyDomains":
    let validPath = "../test/testData/sets/recursive/markerMarkerMarker"
    init(validPath)
    
    test "prettyDomainUpdateRoot":
        let expected0 = """
        {
        "vars": [
          {
            "name": "y",
            "rng": "int(1..9)"
          },
          {
            "name": "s",
            "Cardinality": "int(2..16)",
            "Children": {
              "children": [
                {
                  "name": "s-1",
                  "_children": []
                },
                {
                  "name": "s-2",
                  "_children": []
                }
              ]
            }
          },
          {
            "name": "z",
            "rng": "int(1..9)"
          },
          {
            "name": "x",
            "rng": "int(1..9)"
          }
        ],
        "changed": [],
        "changedExpressions": []
      }
      """
        let pretty0 = loadPrettyDomains("0", "")
        # echo pretty0.pretty()
        check(pretty0 == parseJson(expected0))

    test "prettyDomainUpdateNode1":

        let expected1 = """{
        "vars": [
          {
            "name": "y",
            "rng": "int(1..9)"
          },
          {
            "name": "s",
            "Cardinality": "int(2..16)",
            "Children": {
              "children": [
                {
                  "name": "s-1",
                  "_children": []
                },
                {
                  "name": "s-2",
                  "_children": []
                }
              ]
            }
          },
          {
            "name": "z",
            "rng": "int(1..9)"
          },
          {
            "name": "x",
            "rng": "int(1)"
          }
        ],
        "changed": [
            "x"
        ],
        "changedExpressions": []
        }"""

        let pretty1 = loadPrettyDomains("1", "")
        # echo pretty1.pretty()
        check(pretty1 == parseJson(expected1))

    test "prettyDomainUpdateExpandedChild":

        let expected2 = """{
        "vars": [
          {
            "name": "y",
            "rng": "int(1)"
          },
          {
            "name": "s",
            "Cardinality": "int(2..16)",
            "Children": {
              "children": [
                {
                  "name": "s-1",
                  "_children": []
                },
                {
                  "name": "s-2",
                  "_children": []
                }
              ]
            }
          },
          {
            "name": "z",
            "rng": "int(1..9)"
          },
          {
            "name": "x",
            "rng": "int(1)"
          },
          {
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
          }
        ],
        "changed": [
            "y"
        ],
        "changedExpressions": []
        }"""

        let pretty2 = loadPrettyDomains("2", "s.s-1")
        # echo pretty2.pretty()
        check(pretty2 == parseJson(expected2))

    test "collapsedSetsAppearInChangedList":
        let pretty5 = loadPrettyDomains("5", "")
        check(%"s-1" in pretty5["changed"].getElems())


    test "getExpandedSetChildFailed":
        check(getExpandedSetChild("2","s.3.ASDASD") == nil)

    test "getExpandedSetChildSuccess":
        # check(getExpandedSetChild("2","s.3.ASDASD") == nil)
        check(getExpandedSetChild("2","s.s-1") != nil)

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

        check(loadSetChild("2","s.s-1") == parseJson(expected))

suite "dontCrash":
    let validPath = "../test/testData/sets/recursive/markerMarkerFlags"
    init(validPath)

    test "bug1":
        let pretty1 = loadPrettyDomains("1", "")

    test "bug2":
        let pretty1 = loadPrettyDomains("1", "s.s-1:s.s-1.s-1-1:s.s-2:s.s-2.s-2-1")
    
