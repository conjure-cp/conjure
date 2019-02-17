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

    test "fromZero":
        let nodes = loadNodes("3", "0")
        check(nodes.len() == 3)
        check(nodes[0].nodeId == 1)
        check(nodes[0].parentId == -1)
        check(nodes[0].label == "x = 1")

        check(nodes[1].nodeId == 2)
        check(nodes[1].parentId == 1)
        check(nodes[1].label == "y = 1")

        check(nodes[2].nodeId == 3)
        check(nodes[2].parentId == 2)
        check(nodes[2].label == "z = 1")
        

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
        check(core.len() == 9)

        for i in countUp(0, 7):
            check(core[i].nodeId == i+1)
            check(core[i].children[0] == i+2)

        check(core[0].label == "x = 1")
        check(core[1].label == "y = 1")
        check(core[2].label == "z = 1")

suite "getLabel":
    test "simple":
        check(getLabel("x", "0", "0") == "x != 0")
        check(getLabel("y", "4", "1") == "y = 4")

suite "loadSimpleDomains":
    let validPath = "../test/testData/sets/recursive/markerMarkerMarker"
    init(validPath)
    test "changed":
        var response = loadSimpleDomains("1")
        check(response.changedNames.len() == 0)

        response = loadSimpleDomains("2")
        check(response.changedNames == @["y"])

        response = loadSimpleDomains("3")
        check(response.changedNames == @["z"])

        response = loadSimpleDomains("4")
        check(response.changedNames == @["s_ExplicitVarSizeWithMarkerR5R5_Marker"])

    test "expressions":
        check(loadSimpleDomains("1", true).vars.len() > loadSimpleDomains("1", false).vars.len())

    test "prettyDomainUpdateRoot":
        let expected1 = """{
        "name": "Items",
        "children": [
          {
            "name": "Domain Variables",
            "children": [
              {
                "name": "y",
                "children": [
                  {
                    "name": "int(1..9)",
                    "children": []
                  }
                ]
              },
              {
                "name": "s",
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
                        "name": "int(2..16)",
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
              {
                "name": "z",
                "children": [
                  {
                    "name": "int(1..9)",
                    "children": []
                  }
                ]
              },
              {
                "name": "x",
                "children": [
                  {
                    "name": "int(1)",
                    "children": []
                  }
                ]
              }
            ]
          },
          {
            "name": "Expressions",
            "children": []
          },
          {
            "name": "Changed Expressions",
            "children": []
          }
        ]
        }"""

        let pretty1 = loadPrettyDomains("1", "")
        check(pretty1 == parseJson(expected1))

    test "prettyDomainUpdateNode2":

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
          }
        ],
        "changed": [
          "liDomain Variablesy",
          "liItemsDomain Variables",
          "liItems"
        ],
        "changedExpressions": []
        }"""

        let pretty2 = loadPrettyDomains("2", "")
        check(pretty2 == parseJson(expected2))

    test "prettyDomainUpdateExpandedChild":

        let expected3 = """{
        "vars": [
          {
            "name": "s-1",
            "Cardinality": "int(1..4)"
          },
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
          }
        ],
        "changed": [
          "liDomain Variablesy",
          "liItemsDomain Variables",
          "liItems"
        ],
        "changedExpressions": []
        }"""

        let pretty3 = loadPrettyDomains("2", "s.s-1")

    test "prettyDomainUpdateRemovedChild":

        let expected4 = """{
        "vars": [
          {
            "name": "blah",
            "removed": true
          },
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
          }
        ],
        "changed": [
          "liDomain Variablesy",
          "liItemsDomain Variables",
          "liItems"
        ],
        "changedExpressions": []
        }"""

        let pretty4 = loadPrettyDomains("2", "s.blah")

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

    
