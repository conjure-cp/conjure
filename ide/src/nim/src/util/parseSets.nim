import types, setUtils/marker, setUtils/flags, setUtils/common
import re, strutils, os, tables, json, db_sqlite, parseutils, intsets

proc parseFlags(db: DbConn, s, parent: Set, outerSetName, nodeId: string,
        ancestors: seq[int])
proc parseMarker(db: DbConn, s, parent: Set, outerSetName, nodeId: string,
        ancestors: seq[int])
func parseOccurrence(db: DbConn, s, parent: Set, outerSetName, nodeId: string,
        ancestors: seq[int])
func parseDummy(db: DbConn, s, parent: Set, outerSetName, nodeId: string,
        ancestors: seq[int])
proc parseExplicit(db: DbConn, s, parent: Set, outerSetName, nodeId: string,
        ancestors: seq[int])

proc decideSet*(db: DbConn, s, parent: Set, outerSetName, nodeId: string,
        ancestors: seq[int]) =
    if s of FlagSet:
        parseFlags(db, s, parent, outerSetName, nodeId, ancestors)
    if s of MarkerSet:
        parseMarker(db, s, parent, outerSetName, nodeId, ancestors)
    if s of OccurrenceSet:
        parseOccurrence(db, s, parent, outerSetName, nodeId, ancestors)
    if s of DummySet:
        parseDummy(db, s, parent, outerSetName, nodeId, ancestors)
    if s of ExplicitSet:
        parseExplicit(db, s, parent, outerSetName, nodeId, ancestors)


func parseDummy(db: DbConn, s, parent: Set, outerSetName, nodeId: string,
        ancestors: seq[int]) =
    let d = DummySet(s)
    var query = getInnerSetQuery(d, parent, ancestors, outerSetName)

    for res in db.fastRows(sql(query), nodeId):

        var lower: int
        var upper: int
        discard res[1].parseInt(lower)
        discard res[2].parseInt(upper)

        if lower != d.dummyVal:
            if lower == upper:
                d.includeInSet(lower)

            for i in countUp(lower, min(upper, d.dummyVal - 1)):
                d.dontExclude(i)
        else:
            d.excludedCount.inc()

func parseOccurrence(db: DbConn, s, parent: Set, outerSetName, nodeId: string, ancestors: seq[int]) =
    var query = getInnerSetQuery(s, parent, ancestors, outerSetName)

    for res in db.fastRows(sql(query), nodeId):

        var lower: int
        discard res[0].parseInt(lower)

        if (res[1] == "1" and res[2] == "1"):
            s.includeInSet(lower)

        if (res[2] != "0"):
            s.dontExclude(lower)

proc parseExplicit(db: DbConn, s, parent: Set, outerSetName, nodeId: string,
        ancestors: seq[int]) =
    let e = ExplicitSet(s)

    for setId in countUp(1, e.cardinality):

        if (e.inner != nil):
            let childSet = makeChildSet(e, setID)
            var copy = ancestors
            copy.add(setId)
            decideSet(db, childSet, e, outerSetName, nodeId, copy)
        else:
            var query = getInnerSetQuery(e, parent, ancestors, outerSetName)
            for res in db.rows(sql(query), nodeId):
                var lower: int
                var upper: int
                discard res[1].parseInt(lower)
                discard res[2].parseInt(upper)

                if (lower == upper):
                    e.includeInSet(lower)

                for i in countUp(lower, upper):
                    e.dontExclude(i)
            break

proc parseFlags(db: DbConn, s, parent: Set, outerSetName, nodeId: string, ancestors: seq[int]) =

    var lowerQuery = getTrueFlagCountQuery(ancestors, outerSetName)
    var lowerBound = db.getValue(sql(lowerQuery), nodeId)

    if lowerBound == "":
        lowerBound = "0"

    discard lowerBound.parseInt(s.markerLower)

    var nonExcludedQuery = getNonFalseFlagCountQuery(ancestors, outerSetName)
    var nonExcludedCount = db.getValue(sql(nonExcludedQuery), nodeId)

    discard nonExcludedCount.parseInt(s.notExcludedCount)

    for setId in countUp(1, s.markerLower):

        if (s.inner != nil):
            let childSet = makeChildSet(s, setID)
            var copy = ancestors
            copy.add(setId)
            decideSet(db, childSet, s, outerSetName, nodeId, copy)
        else:
            let valuesQuery = getFlagValuesIncludedQuery(s, ancestors, outerSetName)
            includeValues(db, s, valuesQuery, nodeId)
            
            # s.dontExclude(s.included)

            var falseQuery = getFalseFlagCountQuery(ancestors, outerSetName)
            var falseFlagCount = db.getValue(sql(falseQuery), nodeId)

            discard falseFlagCount.parseInt(s.markerUpper)

            let nonExcludedValuesQuery = getNonExcludedFlagValuesQuery(s, ancestors, outerSetName)

            dontExcludeValues(db, s, nonExcludedValuesQuery, nodeId)
            break;

proc parseMarker(db: DbConn, s, parent: Set, outerSetName, nodeId: string, ancestors: seq[int]) =

    let markerQuery = getMarkerQuery(ancestors, outerSetName)
    var res = db.getRow(sql(markerQuery), nodeId)

    discard res[0].parseInt(s.markerLower)
    discard res[1].parseInt(s.markerUpper)

    for setId in countUp(1, s.markerLower):

        if (s.inner != nil):
            let childSet = makeChildSet(s, setId)
            var copy = ancestors
            copy.add(setId)
            decideSet(db, childSet, s, outerSetName, nodeId, copy)
        else:
            let valuesQuery = getMarkerValuesQuery(ancestors, s.markerLower,
                    outerSetName)
            includeValues(db, s, valuesQuery, nodeId)
            
            let nonExcludedValuesQuery = getNonExcludedMarkerValuesQuery(s, ancestors, outerSetName)

            dontExcludeValues(db, s, nonExcludedValuesQuery, nodeId)
            break;
