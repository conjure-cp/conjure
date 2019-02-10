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


    test "Flags":
        init(pathPrefix & "flags")
        for d in getPrettyDomainsOfNode(db, "21"):
            if d of FlagSet:
                let fS = cast[FlagSet](d)
                check(fS.included == @[1,2,3,7])
                check(fS.excluded.len() == 0)
                check(fS.getCardinality() == "int(5)")

    test "Marker":
        init(pathPrefix & "marker")
        for d in getPrettyDomainsOfNode(db, "3"):
            if d of MarkerSet:
                let mS = cast[MarkerSet](d)
                check(mS.included.len() == 0)
                check(mS.excluded.len() == 0)
                check(mS.getCardinality() == "int(1..5)")

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
    test "MarkerMarkerOccurrence":
        init(pathPrefix & "recursive/markerMarkerOccurrence")

        for d in getPrettyDomainsOfNode(db, "7"):
            # echo d
            if (d of MarkerSet):
                let mS = cast[MarkerSet](d)

                # echo mS.children

                check(mS.inner of MarkerSet)
                check(mS.getCardinality() == "int(2)")

                check(mS.inner.inner of OccurrenceSet)

                check(mS.children[0].getCardinality() == "int(1)")
                check(mS.children[0].children[0].getCardinality() == "int(1)")
                check(mS.children[0].children[0].included.len() == 0)
                check(mS.children[0].children[0].excluded == @[1])

                check(mS.children[1].getCardinality() == "int(1)")
                check(mS.children[1].children[0].getCardinality() == "int(1)")
                check(mS.children[1].children[0].included.len() == 0)
                check(mS.children[1].children[0].excluded == @[1])


                echo ms.children[1].children[0].name

    test "MarkerMarkerMarker":
        init(pathPrefix & "recursive/markerMarkerMarker")

        for d in getPrettyDomainsOfNode(db, "2"):
            if (d of MarkerSet):
                let mS = cast[MarkerSet](d)
                # check(mS.children.len() == 2)
                # check(mS.children[0].children.len() == 1)
                # check(mS.children[1].children.len() == 1)

        # for d in getPrettyDomainsOfNode(db, "8"):
        #     if (d of MarkerSet):
        #         let mS = cast[MarkerSet](d)
        #         check(mS.children.len() == 2)
        #         check(mS.children[0].children.len() == 1)
        #         check(mS.children[1].children.len() == 1)

        #         check(mS.inner of MarkerSet)