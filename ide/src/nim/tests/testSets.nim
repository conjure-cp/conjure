import unittest
include util/main

let pathPrefix = "../test/testData/sets/"

suite "level1":

    test "Occurrence":
        init(pathPrefix & "occurrence")
        for d in getPrettyDomainsOfNode(db, "11"):
            if d of OccurrenceSet:
                let oS = cast[OccurrenceSet](d)
                check(oS.included == @[9, 4])
                check(oS.excluded == @[2, 8, 3])
                check(os.getCardinality() == "int(2..6)")

    test "Dummy":
        init(pathPrefix & "dummy")
        for d in getPrettyDomainsOfNode(db, "10"):
            if d of DummySet:
                let dS = cast[DummySet](d)
                check(dS.included == @[6, 7, 1])
                check(dS.excluded.len() == 0)
                check(dS.getCardinality() == "int(3..4)")


    test "Explicit":
        init(pathPrefix & "explicit")
        for d in getPrettyDomainsOfNode(db, "3"):
            if d of ExplicitSet:
                let eS = cast[DummySet](d)
                check(eS.included == @[1, 2, 6])
                check(eS.excluded.len() == 0)
                check(eS.getCardinality() == "int(4)")


suite "level2":

    test "ExplicitOccurrence":

        init(pathPrefix & "recursive/explicitOccurrence")

        for d in getPrettyDomainsOfNode(db, "4"):
            # echo d
            if (d of ExplicitSet):
                let eS = cast[ExplicitSet](d)
                check(eS.getCardinality() == "int(2)")
                check(eS.inner of OccurrenceSet)

                check(eS.children[0].included.len() == 0)
                check(eS.children[0].excluded == @[2,4,5,6,7,8,9])
                check(eS.children[0].getCardinality() == "int(1..2)")

                check(eS.children[1].included.len() == 0)
                check(eS.children[1].excluded == @[4,5,6,7,8])
                check(eS.children[1].getCardinality() == "int(1..4)")

    test "ExplicitExplicit":

        init(pathPrefix & "recursive/explicitExplicit")

        for d in getPrettyDomainsOfNode(db, "2"):
            # echo d
            if (d of ExplicitSet):
                let eS = cast[ExplicitSet](d)
                check(eS.inner of ExplicitSet)
                check(eS.getCardinality() == "int(2)")

                check(es.children[0] of ExplicitSet)
                check(es.children[0].getCardinality() == "int(1)")
                check(es.children[0].included.len() == 0)
                check(es.children[0].excluded.len() == 0)
                # check(es.children[0] of ExplicitSet)
                check(es.children[1] of ExplicitSet)
                check(es.children[1].getCardinality() == "int(1)")
                check(es.children[1].included.len() == 0)
                check(es.children[1].excluded.len() == 0)

    test "MarkerOccurrence":
        init(pathPrefix & "recursive/markerOccurrence")

        for d in getPrettyDomainsOfNode(db, "4"):
            # echo d
            if (d of MarkerSet):
                let mS = cast[MarkerSet](d)
                check(mS.inner of OccurrenceSet)
                check(mS.getCardinality() == "int(1)")

                check(mS.children[0].included.len() == 0)
                check(mS.children[0].excluded == @[4, 5, 6, 7, 8, 9])
                check(mS.children[0].getCardinality() == "int(1..3)")

    test "MarkerDummy":
        init(pathPrefix & "recursive/markerDummy")

        for d in getPrettyDomainsOfNode(db, "5"):
            # echo d
            if (d of MarkerSet):
                let mS = cast[MarkerSet](d)
                check(mS.inner of DummySet)
                check(mS.getCardinality() == "int(1)")

                check(mS.children[0].included == @[1])
                check(mS.children[0].excluded.len() == 0)
                check(mS.children[0].getCardinality() == "int(1..9)")


    test "MarkerMarker":
        init(pathPrefix & "recursive/markerMarker")

        for d in getPrettyDomainsOfNode(db, "4"):
            # echo d
            if (d of MarkerSet):
                let mS = cast[MarkerSet](d)
                check(mS.inner of MarkerSet)
                check(mS.getCardinality() == "int(1)")

                check(mS.children[0].included == @[1, 2])
                check(mS.children[0].excluded.len() == 0)
                check(mS.children[0].getCardinality() == "int(2)")

    test "MarkerFlags":
        init(pathPrefix & "recursive/markerFlags")

        for d in getPrettyDomainsOfNode(db, "4"):
            # echo d
            if (d of MarkerSet):
                let mS = cast[MarkerSet](d)
                check(mS.inner of FlagSet)
                check(mS.getCardinality() == "int(1)")
                echo mS
                check(mS.children[0].included.len() == 0)
                check(mS.children[0].excluded.len() == 0)
                check(mS.children[0].getCardinality() == "int(0)")

    test "MarkerExplicit":
        init(pathPrefix & "recursive/markerExplicit")

        for d in getPrettyDomainsOfNode(db, "3"):
            # echo d
            if (d of MarkerSet):
                let mS = cast[MarkerSet](d)
                check(mS.inner of ExplicitSet)
                check(mS.getCardinality() == "int(1..2)")

                check(mS.children[0].included.len() == 0)
                check(mS.children[0].excluded.len() == 0)
                check(mS.children[0].getCardinality() == "int(2)")

    
    

suite "level3":
    break
    # test "MarkerMarkerOccurrence":
    #     init(pathPrefix & "recursive/markerMarkerOccurrence")

    #     for d in getPrettyDomainsOfNode(db, "7"):
    #         # echo d
    #         if (d of MarkerSet):
    #             let mS = cast[MarkerSet](d)

    #             # echo mS.children

    #             check(mS.inner of MarkerSet)
    #             check(mS.getCardinality() == "int(2)")

    #             check(mS.inner.inner of OccurrenceSet)

    #             check(mS.children[0].getCardinality() == "int(1)")
    #             check(mS.children[0].children[0].getCardinality() == "int(1)")
    #             check(mS.children[0].children[0].included.len() == 0)
    #             check(mS.children[0].children[0].excluded == @[1])

    #             check(mS.children[1].getCardinality() == "int(1)")
    #             check(mS.children[1].children[0].getCardinality() == "int(1)")
    #             check(mS.children[1].children[0].included.len() == 0)
    #             check(mS.children[1].children[0].excluded == @[1])

    #             echo ms.children[1].children[0].name

# suite "level4":

suite "temp":
    test "MMMM":
        init(pathPrefix & "recursive/markerMarkerMarkerMarker")
        for d in getPrettyDomainsOfNode(db, "2"):
            if (d of MarkerSet):
                let mS = cast[MarkerSet](d)

                echo mS

                let child1 = ms.children[0]
                let grandkid1 = child1.children[0]
                let greatGrandKid1 = grandkid1.children[0]
                check(greatGrandKid1.included == @[1])

    test "FFFF":
        init(pathPrefix & "recursive/ffff")
        for d in getPrettyDomainsOfNode(db, "2"):
            if (d of FlagSet):
                let s = cast[FlagSet](d)
                echo s
                let child1 = s.children[0]
                let grandkid1 = child1.children[0]
                let greatGrandKid1 = grandkid1.children[0]
                check(greatGrandKid1.included == @[1])

    test "FFF":
        init(pathPrefix & "recursive/flagsFlagsFlags")

        for d in getPrettyDomainsOfNode(db, "15"):
            if (d of FlagSet):
                let s = cast[FlagSet](d)

                echo s

                let child1 = s.children[0]
                check(child1 of FlagSet)

                let grandkid1 = child1.children[0]
                check(grandkid1 of FlagSet)
                check(grandkid1.included == @[1])

                let child2 = s.children[1]
                check(child2 of FlagSet)

                let grandkid2 = child2.children[0]
                check(grandkid2.included == @[2])
                check(grandkid2 of FlagSet)

    test "FFM":
        init(pathPrefix & "recursive/flagsFlagsMarker")

        for d in getPrettyDomainsOfNode(db, "13"):
            if (d of FlagSet):
                let s = cast[FlagSet](d)

                echo s

                let child1 = s.children[0]
                check(child1 of FlagSet)

                let grandkid1 = child1.children[0]
                check(grandkid1 of MarkerSet)
                check(grandkid1.included == @[1])

                let child2 = s.children[1]
                check(child2 of FlagSet)

                let grandkid2 = child2.children[0]
                check(grandkid2.included == @[2])
                check(grandkid2 of MarkerSet)

    test "FMF":
        init(pathPrefix & "recursive/flagsMarkerFlags")

        for d in getPrettyDomainsOfNode(db, "13"):
            if (d of FlagSet):
                let s = cast[FlagSet](d)

                echo s

                let child1 = s.children[0]
                check(child1 of MarkerSet)

                let grandkid1 = child1.children[0]
                check(grandkid1 of FlagSet)
                check(grandkid1.included == @[1])

                let child2 = s.children[1]
                check(child2 of MarkerSet)

                let grandkid2 = child2.children[0]
                check(grandkid2.included == @[2])
                check(grandkid2 of FlagSet)

    test "MFF":
        init(pathPrefix & "recursive/markerFlagsFlags")

        for d in getPrettyDomainsOfNode(db, "13"):
            if (d of MarkerSet):
                let s = cast[MarkerSet](d)

                echo s

                let child1 = s.children[0]
                check(child1 of FlagSet)

                let grandkid1 = child1.children[0]
                check(grandkid1 of FlagSet)
                check(grandkid1.included == @[1])

                let child2 = s.children[1]
                check(child2 of FlagSet)

                let grandkid2 = child2.children[0]
                check(grandkid2.included == @[2])
                check(grandkid2 of FlagSet)

    test "MMF":
        init(pathPrefix & "recursive/markerMarkerFlags")

        for d in getPrettyDomainsOfNode(db, "11"):
            if (d of MarkerSet):
                let s = cast[MarkerSet](d)

                echo s

                let child1 = s.children[0]
                check(child1 of MarkerSet)

                let grandkid1 = child1.children[0]
                check(grandkid1 of FlagSet)
                check(grandkid1.included == @[1])

                let child2 = s.children[1]
                check(child2 of MarkerSet)

                let grandkid2 = child2.children[0]
                check(grandkid2.included == @[2])
                check(grandkid2 of FlagSet)

    test "FMM":
        init(pathPrefix & "recursive/flagsMarkerMarker")

        for d in getPrettyDomainsOfNode(db, "11"):
            if (d of FlagSet):
                let s = cast[FlagSet](d)

                echo s

                let child1 = s.children[0]
                check(child1 of MarkerSet)

                let grandkid1 = child1.children[0]
                check(grandkid1 of MarkerSet)
                check(grandkid1.included == @[1])

                let child2 = s.children[1]
                check(child2 of MarkerSet)

                let grandkid2 = child2.children[0]
                check(grandkid2.included == @[2])
                check(grandkid2 of MarkerSet)


    test "MMM":
        init(pathPrefix & "recursive/markerMarkerMarker")

        for d in getPrettyDomainsOfNode(db, "9"):
            if (d of MarkerSet):
                let s = cast[MarkerSet](d)

                echo s

                let child1 = s.children[0]
                check(child1 of MarkerSet)

                let grandkid1 = child1.children[0]
                check(grandkid1.included == @[1])
                check(grandkid1 of MarkerSet)

                let child2 = s.children[1]
                check(child2 of MarkerSet)

                let grandkid2 = child2.children[0]
                check(grandkid2.included == @[2])
                check(grandkid2 of MarkerSet)



    test "FF":
        init(pathPrefix & "recursive/flagsFlags")

        for d in getPrettyDomainsOfNode(db, "8"):
            if (d of FlagSet):
                let s = cast[FlagSet](d)
                check(s.getCardinality() == "int(1)")

                let child1 = s.children[0]
                check(child1 of FlagSet)
                check(child1.included == @[3])
                check(child1.excluded.len() == 0)
                check(child1.getCardinality() == "int(1)")

    test "F":
        echo pathPrefix & "flags"
        init(pathPrefix & "flags")
        for d in getPrettyDomainsOfNode(db, "7"):
            if d of FlagSet:
                let fS = cast[FlagSet](d)
                check(fS.included == @[1,2,3])
                check(fS.excluded.len() == 0)
                check(fS.getCardinality() == "int(3)")
                
    test "M":
        init(pathPrefix & "marker")
        for d in getPrettyDomainsOfNode(db, "4"):
            if d of MarkerSet:
                let mS = cast[MarkerSet](d)
                check(mS.included == @[6])
                check(mS.excluded.len() == 0)
                check(mS.getCardinality() == "int(1)")

    test "MM":
        init(pathPrefix & "recursive/markerMarker")

        for d in getPrettyDomainsOfNode(db, "5"):
            # echo d
            if (d of MarkerSet):
                let s = cast[MarkerSet](d)
                check(s.getCardinality() == "int(1)")

                let child1 = s.children[0]
                check(child1 of MarkerSet)

                check(child1.included == @[1, 2])
                check(child1.excluded.len() == 0)
                check(child1.getCardinality() == "int(2)")
    
    test "MF":
        init(pathPrefix & "recursive/markerFlags")
        for d in getPrettyDomainsOfNode(db, "8"):
            # echo d
            if (d of MarkerSet):
                let s = cast[MarkerSet](d)
                check(s.getCardinality() == "int(1)")

                echo s

                let child1 = s.children[0]
                check(child1 of FlagSet)
                check(child1.included == @[3])
                check(child1.excluded.len() == 0)
                check(child1.getCardinality() == "int(1)")

    test "FM":
        init(pathPrefix & "recursive/flagsMarker")
        for d in getPrettyDomainsOfNode(db, "8"):
            # echo d
            if (d of FlagSet):
                let s = cast[FlagSet](d)
                check(s.getCardinality() == "int(1)")

                echo s

                let child1 = s.children[0]
                check(child1 of MarkerSet)
                check(child1.included == @[3])
                check(child1.excluded.len() == 0)
                check(child1.getCardinality() == "int(1)")

suite "occurrence":

    test "O":
        init(pathPrefix & "occurrence")
        for d in getPrettyDomainsOfNode(db, "7"):
            if (d of OccurrenceSet):
                let s = cast[OccurrenceSet](d)
                check(s.getCardinality() == "int(1)")
                check(s.included == @[6])
                check(s.excluded == @[1,2,3,4,5,7,8,9])

    test "MO":
        init(pathPrefix & "recursive/markerOccurrence")
        for d in getPrettyDomainsOfNode(db, "7"):
            if (d of MarkerSet):
                let s = cast[MarkerSet](d)
                check(s.getCardinality() == "int(1)")

                let child1 = s.children[0]
                check(child1 of OccurrenceSet)
                check(child1.included == @[3])
                check(child1.excluded == @[1,2])

    test "FO":
        init(pathPrefix & "recursive/flagsOccurrence")
        for d in getPrettyDomainsOfNode(db, "7"):
            if (d of FlagSet):
                let s = cast[FlagSet](d)
                check(s.getCardinality() == "int(1)")
                let child1 = s.children[0]
                check(child1 of OccurrenceSet)
                check(child1.included == @[3])
                check(child1.excluded == @[1,2])

    test "MMO":
        init(pathPrefix & "recursive/markerMarkerOccurrence")

        for d in getPrettyDomainsOfNode(db, "8"):
            if (d of MarkerSet):
                let s = cast[MarkerSet](d)

                echo s

                let child1 = s.children[0]
                check(child1 of MarkerSet)

                let grandkid1 = child1.children[0]
                check(grandkid1 of OccurrenceSet)
                check(grandkid1.included == @[2])
                check(grandkid1.excluded == @[1])

                let child2 = s.children[1]
                check(child2 of MarkerSet)

                let grandkid2 = child2.children[0]
                check(grandkid2 of OccurrenceSet)
                check(grandkid2.included == @[1])
                check(grandkid2.excluded == @[2])

    test "FFO":
        init(pathPrefix & "recursive/flagsFlagsOccurrence")

        for d in getPrettyDomainsOfNode(db, "12"):
            if (d of FlagSet):
                let s = cast[FlagSet](d)

                echo s

                let child1 = s.children[0]
                check(child1 of FlagSet)

                let grandkid1 = child1.children[0]
                check(grandkid1 of OccurrenceSet)
                check(grandkid1.included == @[2])
                check(grandkid1.excluded == @[1])

                let child2 = s.children[1]
                check(child2 of FlagSet)

                let grandkid2 = child2.children[0]
                check(grandkid2 of OccurrenceSet)
                check(grandkid2.included == @[1])
                check(grandkid2.excluded == @[2])

    test "FFFO":
        init(pathPrefix & "recursive/flagsFlagsFlagsOccurrence")

        for d in getPrettyDomainsOfNode(db, "2"):
            if (d of FlagSet):
                let s = cast[FlagSet](d)

                echo s

                let child1 = s.children[0]
                check(child1 of FlagSet)

                let grandkid1 = child1.children[0]
                check(grandkid1 of FlagSet)

                let greatGrandKid1 = grandkid1.children[0]
                check(greatGrandKid1 of OccurrenceSet)
                check(greatGrandKid1.included == @[1])
                check(greatGrandKid1.excluded == @[2])

suite "dummy":

    test "D":
        init(pathPrefix & "dummy")
        for d in getPrettyDomainsOfNode(db, "7"):
            if (d of DummySet):
                let s = cast[DummySet](d)
                check(s.getCardinality() == "int(3)")
                check(s.included == @[1, 2, 3])
                check(s.excluded.len() == 0)

    test "MD":
        init(pathPrefix & "recursive/markerDummy")
        for d in getPrettyDomainsOfNode(db, "7"):
            if (d of MarkerSet):
                let s = cast[MarkerSet](d)
                check(s.getCardinality() == "int(1)")

                let child1 = s.children[0]
                check(child1 of DummySet)
                check(child1.included == @[1,2])
                check(child1.excluded.len() == 0)

    test "FD":
        init(pathPrefix & "recursive/flagsDummy")
        for d in getPrettyDomainsOfNode(db, "7"):
            if (d of FlagSet):
                let s = cast[FlagSet](d)
                check(s.getCardinality() == "int(1)")
                let child1 = s.children[0]
                check(child1 of DummySet)
                check(child1.included == @[1,2])
                check(child1.excluded.len() == 0)

    test "MMD":
        init(pathPrefix & "recursive/markerMarkerDummy")

        for d in getPrettyDomainsOfNode(db, "10"):
            if (d of MarkerSet):
                let s = cast[MarkerSet](d)

                echo s

                let child1 = s.children[0]
                check(child1 of MarkerSet)

                let grandkid1 = child1.children[0]
                check(grandkid1 of DummySet)
                check(grandkid1.included == @[1])
                check(grandkid1.excluded.len() == 0)

                let child2 = s.children[1]
                check(child2 of MarkerSet)

                let grandkid2 = child2.children[0]
                check(grandkid2 of DummySet)
                check(grandkid2.included == @[2])
                check(grandkid2.excluded.len() == 0)

    test "FFD":
        init(pathPrefix & "recursive/flagsFlagsDummy")

        for d in getPrettyDomainsOfNode(db, "14"):
            if (d of FlagSet):
                let s = cast[FlagSet](d)

                echo s

                let child1 = s.children[0]
                check(child1 of FlagSet)

                let grandkid1 = child1.children[0]
                check(grandkid1 of DummySet)
                check(grandkid1.included == @[1])
                check(grandkid1.excluded.len() == 0)

                let child2 = s.children[1]
                check(child2 of FlagSet)

                let grandkid2 = child2.children[0]
                check(grandkid2 of DummySet)
                check(grandkid2.included == @[2])
                check(grandkid2.excluded.len() == 0)


    test "FFFD":
        init(pathPrefix & "recursive/flagsFlagsFlagsDummy")

        for d in getPrettyDomainsOfNode(db, "2"):
            if (d of FlagSet):
                let s = cast[FlagSet](d)

                echo s

                let child1 = s.children[0]
                check(child1 of FlagSet)

                let grandkid1 = child1.children[0]
                check(grandkid1 of FlagSet)

                let greatGrandKid1 = grandkid1.children[0]
                check(greatGrandKid1 of DummySet)
                check(greatGrandKid1.included == @[1])
                check(greatGrandKid1.excluded.len() == 0)
