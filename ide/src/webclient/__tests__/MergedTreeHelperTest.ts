import Node, { WhichTree } from "../src/modules/Node"
import { loadDiffs, mergeMaps, getDescList } from "../src/modules/ForestHelper"
import { fetchAncestors, goLeftBoyo } from "../src/modules/MovementHelper"
// import { FetchMock} from "jest-fetch-mock"
import { cloneDeep } from "lodash"
import * as d3 from "d3"
import { node } from "prop-types"
import {
  goLeftAtDiffingPoint,
  reviseGoLeft,
  shouldBeRightTree
} from "../src/modules/MergedTreeHelper"
import { flipDiffLocations } from "../src/modules/Helper"
import {
  coreOf3 as coreOf3Normal,
  coreOf17 as coreOf17Normal,
  coreOf27 as coreOf27Normal,
  descendantsOf4 as descendantsOf4Normal,
  descendantsOf7 as descendantsOf7Normal,
  descendantsOf18 as descendantsOf18Normal,
  descendantsOf21 as descendantsOf21Normal,
  descendantsOf28 as descendantsOf28Normal,
  core as coreNormal
} from "./resources/normal-8"
import {
  coreOf3 as coreOf3Sacbounds,
  coreOf6 as coreOf6Sacbounds,
  coreOf9 as coreOf9Sacbounds,
  core as coreSacbounds
} from "./resources/sacbounds-8"
import { diffLocations } from "./resources/normalVSSacbounds-8"

describe("suite to test MergedTreeHelper", () => {
  let bigTree: any
  let smallTree: any
  let flipped = flipDiffLocations(diffLocations)

  beforeEach(async () => {
    fetchMock.resetMocks()
    fetchMock
      .once(JSON.stringify(coreOf3Normal))
      .once(JSON.stringify(coreOf17Normal))
      .once(JSON.stringify(coreOf27Normal))
      .once(JSON.stringify(coreOf3Sacbounds))
      .once(JSON.stringify(coreOf6Sacbounds))
      .once(JSON.stringify(coreOf9Sacbounds))
      .once(JSON.stringify(descendantsOf4Normal))
      .once(JSON.stringify(descendantsOf7Normal))
      .once(JSON.stringify(descendantsOf18Normal))
      .once(JSON.stringify(descendantsOf21Normal))
      .once(JSON.stringify(descendantsOf28Normal))

    let res = await loadDiffs(
      ["", "s"],
      [coreNormal, coreSacbounds],
      diffLocations,
      5000
    )

    bigTree = res[0]

    bigTree = (await goLeftBoyo(
      4,
      bigTree,
      false,
      false,
      "",
      10,
      5000,
      WhichTree.Right
    )).id2Node

    bigTree = (await goLeftBoyo(
      7,
      bigTree,
      false,
      false,
      "",
      10,
      5000,
      WhichTree.Right
    )).id2Node

    bigTree = (await goLeftBoyo(
      18,
      bigTree,
      false,
      false,
      "",
      10,
      5000,
      WhichTree.Right
    )).id2Node

    bigTree = (await goLeftBoyo(
      21,
      bigTree,
      false,
      false,
      "",
      10,
      5000,
      WhichTree.Right
    )).id2Node

    bigTree = (await goLeftBoyo(
      28,
      bigTree,
      false,
      false,
      "",
      10,
      5000,
      WhichTree.Right
    )).id2Node

    smallTree = res[1]
  })

  describe("test should be right tree", () => {
    it("returns the correct ids small -> big", async () => {
      for (let i = 3; i < 16; i++) {
        //   console.log(i)
        expect(
          shouldBeRightTree(smallTree, bigTree, i, true, flipped)
        ).toBeTruthy()
      }
      expect(
        shouldBeRightTree(smallTree, bigTree, 16, true, flipped)
      ).toBeFalsy()

      for (let i = 18; i < 26; i++) {
        expect(
          shouldBeRightTree(smallTree, bigTree, i, true, flipped)
        ).toBeTruthy()
      }
      expect(
        shouldBeRightTree(smallTree, bigTree, 26, true, flipped)
      ).toBeFalsy()

      for (let i = 28; i < 33; i++) {
        expect(
          shouldBeRightTree(smallTree, bigTree, i, true, flipped)
        ).toBeTruthy()
      }
    })

    it("returns the correct ids big -> small", async () => {
      expect(
        shouldBeRightTree(bigTree, smallTree, 5, true, flipped)
      ).toBeFalsy()
      expect(
        shouldBeRightTree(bigTree, smallTree, 8, true, flipped)
      ).toBeFalsy()
      expect(
        shouldBeRightTree(bigTree, smallTree, 11, true, flipped)
      ).toBeFalsy()
    })
    it("returns false for all nodes that are not strickly on the right", async () => {
      expect(
        shouldBeRightTree(bigTree, smallTree, 1, false, flipped)
      ).toBeFalsy()
    })
  })
  describe("test revise goleft, it redirects to a node to the right of the next diff location", () => {
    it("returns the correct ids small -> big", async () => {
      const merged = mergeMaps(smallTree, bigTree, flipped)
      let res = reviseGoLeft(merged, 4, 5, WhichTree.Left, flipped)
      expect(res).toEqual({ selected: 5, treeId: WhichTree.Both })

      res = reviseGoLeft(merged, 7, 8, WhichTree.Left, flipped)
      expect(res).toEqual({ selected: 8, treeId: WhichTree.Both })

      res = reviseGoLeft(merged, 10, 11, WhichTree.Left, flipped)
      expect(res).toEqual({ selected: 10, treeId: WhichTree.Left })
    })

    it("returns the correct ids big -> small", async () => {
      const merged = mergeMaps(bigTree, smallTree, diffLocations)

      let res = reviseGoLeft(merged, 4, 5, WhichTree.Right, diffLocations)
      expect(res).toEqual({ selected: 16, treeId: WhichTree.Both })

      res = reviseGoLeft(merged, 7, 8, WhichTree.Right, diffLocations)
      expect(res).toEqual({ selected: 26, treeId: WhichTree.Both })

      res = reviseGoLeft(merged, 10, 11, WhichTree.Right, diffLocations)
      expect(res).toEqual({ selected: 10, treeId: WhichTree.Right })

      res = reviseGoLeft(merged, 0, 2, WhichTree.Both, diffLocations)
      expect(res).toEqual({ selected: 2, treeId: WhichTree.Both })
    })
  })

  describe("test go left at diffing point", () => {
    it("returns the correct ids small -> big", async () => {
      let mergeMap = await mergeMaps(bigTree, smallTree, diffLocations)
      expect(goLeftAtDiffingPoint(mergeMap, 3)).toEqual({
        selected: 4,
        selectedTreeId: WhichTree.Left
      })

      expect(goLeftAtDiffingPoint(mergeMap, 17)).toEqual({
        selected: 18,
        selectedTreeId: WhichTree.Left
      })

      expect(goLeftAtDiffingPoint(mergeMap, 27)).toEqual({
        selected: 28,
        selectedTreeId: WhichTree.Left
      })
    })

    it("returns the correct ids big -> small", async () => {
      let mergeMap = await mergeMaps(smallTree, bigTree, flipped)
      expect(goLeftAtDiffingPoint(mergeMap, 3)).toEqual({
        selected: 4,
        selectedTreeId: WhichTree.Left
      })

      expect(goLeftAtDiffingPoint(mergeMap, 6)).toEqual({
        selected: 7,
        selectedTreeId: WhichTree.Left
      })

      expect(goLeftAtDiffingPoint(mergeMap, 9)).toEqual({
        selected: 10,
        selectedTreeId: WhichTree.Left
      })
    })
  })
})
