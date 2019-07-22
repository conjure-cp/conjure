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

const flipped = flipDiffLocations(diffLocations)

async function loadTreeBigOnLeftSmallOnRight() {
  let bigTree: any
  let smallTree: any

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

  return { smallTree, bigTree }
}
async function loadTreeSmallOnLeftBigOnRight() {
  fetchMock.resetMocks()
  fetchMock
    .once(JSON.stringify(coreOf3Sacbounds))
    .once(JSON.stringify(coreOf6Sacbounds))
    .once(JSON.stringify(coreOf9Sacbounds))
    .once(JSON.stringify(coreOf3Normal))
    .once(JSON.stringify(coreOf17Normal))
    .once(JSON.stringify(coreOf27Normal))

    .once(JSON.stringify(descendantsOf4Normal))
    .once(JSON.stringify(descendantsOf7Normal))
    .once(JSON.stringify(descendantsOf18Normal))
    .once(JSON.stringify(descendantsOf21Normal))
    .once(JSON.stringify(descendantsOf28Normal))

  let res = await loadDiffs(
    ["", "s"],
    [coreSacbounds, coreNormal],
    flipped,
    5000
  )

  res[1] = (await goLeftBoyo(
    4,
    res[1],
    false,
    false,
    "",
    10,
    5000,
    WhichTree.Right
  )).id2Node

  res[1] = (await goLeftBoyo(
    7,
    res[1],
    false,
    false,
    "",
    10,
    5000,
    WhichTree.Right
  )).id2Node

  res[1] = (await goLeftBoyo(
    18,
    res[1],
    false,
    false,
    "",
    10,
    5000,
    WhichTree.Right
  )).id2Node

  res[1] = (await goLeftBoyo(
    21,
    res[1],
    false,
    false,
    "",
    10,
    5000,
    WhichTree.Right
  )).id2Node

  res[1] = (await goLeftBoyo(
    28,
    res[1],
    false,
    false,
    "",
    10,
    5000,
    WhichTree.Right
  )).id2Node

  return { smallTree: res[0], bigTree: res[1] }
}

describe("suite to test MergedTreeHelper", () => {
  let bigTree: any
  let smallTree: any

  // describe("test should be right tree", () => {
  describe("Left: small | Right: big", () => {
    beforeEach(async () => {
      let res = await loadTreeSmallOnLeftBigOnRight()
      bigTree = res.bigTree
      smallTree = res.smallTree
    })
    it("should be false for going to 3", async () => {
      expect(shouldBeRightTree(smallTree, bigTree, 3, false)).toBeFalsy()
    })
    it("should be false for going to 6", async () => {
      expect(shouldBeRightTree(smallTree, bigTree, 6, false)).toBeFalsy()
    })

    it("should be false for going to 9", async () => {
      expect(shouldBeRightTree(smallTree, bigTree, 9, false)).toBeFalsy()
    })

    it("should be false for going from 4 to 15", async () => {
      for (let i = 4; i < 16; i++) {
        expect(shouldBeRightTree(smallTree, bigTree, i, true)).toBeTruthy()
      }
    })
    it("should be false for going to 16", async () => {
      expect(shouldBeRightTree(smallTree, bigTree, 16, true)).toBeFalsy()
    })
    it("should be false for going from 18 to 25", async () => {
      for (let i = 18; i < 26; i++) {
        expect(shouldBeRightTree(smallTree, bigTree, i, true)).toBeTruthy()
      }
    })
    it("should be false for going to 26", async () => {
      expect(shouldBeRightTree(smallTree, bigTree, 26, true)).toBeFalsy()
    })

    it("should be false for going from 28 to 32", async () => {
      for (let i = 28; i < 33; i++) {
        expect(shouldBeRightTree(smallTree, bigTree, i, true)).toBeTruthy()
      }
    })
  })
  describe("Left: big | Right: small", () => {
    beforeEach(async () => {
      let res = await loadTreeBigOnLeftSmallOnRight()
      bigTree = res.bigTree
      smallTree = res.smallTree
    })
    it("should return false for trying to go to 5", async () => {
      expect(shouldBeRightTree(bigTree, smallTree, 5, true)).toBeFalsy()
    })

    it("should return false for trying to go to 8", async () => {
      expect(shouldBeRightTree(bigTree, smallTree, 8, true)).toBeFalsy()
    })
    it("should return false for trying to go to 11", async () => {
      expect(shouldBeRightTree(bigTree, smallTree, 11, true)).toBeFalsy()
    })
  })

  describe("test revise goleft, it redirects to a node to the right of the next diff location", () => {
    describe("Left: small | Right: big", () => {
      beforeEach(async () => {
        let res = await loadTreeSmallOnLeftBigOnRight()
        bigTree = res.bigTree
        smallTree = res.smallTree
      })
    })

    it("redirects 4 -> 5 from the left tree", async () => {
      const merged = mergeMaps(smallTree, bigTree, flipped)
      let res = reviseGoLeft(merged, 4, WhichTree.Left, flipped)
      expect(res).toEqual({ selected: 5, treeId: WhichTree.Both })
    })

    it("redirects 7 -> 8 from the left tree", async () => {
      const merged = mergeMaps(smallTree, bigTree, flipped)
      let res = reviseGoLeft(merged, 7, WhichTree.Left, flipped)
      expect(res).toEqual({ selected: 8, treeId: WhichTree.Both })
    })

    it("redirects 10 -> 10 from the left tree", async () => {
      const merged = mergeMaps(smallTree, bigTree, flipped)
      let res = reviseGoLeft(merged, 10, WhichTree.Both, flipped)
      expect(res).toEqual({ selected: 10, treeId: WhichTree.Both })
    })
  })
  describe("Left: big | right: small", () => {
    beforeEach(async () => {
      let res = await loadTreeBigOnLeftSmallOnRight()
      bigTree = res.bigTree
      smallTree = res.smallTree
    })

    it("redirects 4 -> 16", async () => {
      const merged = mergeMaps(bigTree, smallTree, diffLocations)
      let res = reviseGoLeft(merged, 4, WhichTree.Right, diffLocations)
      expect(res).toEqual({ selected: 16, treeId: WhichTree.Both })
    })

    it("redirects 7 -> 26", async () => {
      const merged = mergeMaps(bigTree, smallTree, diffLocations)
      let res = reviseGoLeft(merged, 7, WhichTree.Right, diffLocations)
      expect(res).toEqual({ selected: 26, treeId: WhichTree.Both })
    })

    it("redirects 10 ->  10", async () => {
      const merged = mergeMaps(bigTree, smallTree, diffLocations)
      let res = reviseGoLeft(merged, 10, WhichTree.Right, diffLocations)
      expect(res).toEqual({ selected: 10, treeId: WhichTree.Right })
    })
  })

  describe("test go left at diffing point", () => {
    describe("Left: small | Right: big", () => {
      beforeEach(async () => {
        let res = await loadTreeSmallOnLeftBigOnRight()
        bigTree = res.bigTree
        smallTree = res.smallTree
      })
    })
    it("redirects from 3 -> 4 on the left tree", async () => {
      let mergeMap = await mergeMaps(bigTree, smallTree, diffLocations)
      expect(goLeftAtDiffingPoint(mergeMap, 3)).toEqual({
        selected: 4,
        selectedTreeId: WhichTree.Left
      })
    })
    it("redirects from 17 -> 18 on the left tree", async () => {
      let mergeMap = await mergeMaps(bigTree, smallTree, diffLocations)
      expect(goLeftAtDiffingPoint(mergeMap, 17)).toEqual({
        selected: 18,
        selectedTreeId: WhichTree.Left
      })
    })
    it("redirects from 27 -> 28 on the left tree", async () => {
      let mergeMap = await mergeMaps(bigTree, smallTree, diffLocations)
      expect(goLeftAtDiffingPoint(mergeMap, 27)).toEqual({
        selected: 28,
        selectedTreeId: WhichTree.Left
      })
    })

    describe("Left: Big | Right: small", () => {
      beforeEach(async () => {
        let res = await loadTreeBigOnLeftSmallOnRight()
        bigTree = res.bigTree
        smallTree = res.smallTree
      })
      it("redirects from 3 -> 4 ", async () => {
        let mergeMap = await mergeMaps(smallTree, bigTree, flipped)
        expect(goLeftAtDiffingPoint(mergeMap, 3)).toEqual({
          selected: 4,
          selectedTreeId: WhichTree.Left
        })
      })
      it("redirects from 3 -> 4 ", async () => {
        let mergeMap = await mergeMaps(smallTree, bigTree, flipped)
        expect(goLeftAtDiffingPoint(mergeMap, 6)).toEqual({
          selected: 7,
          selectedTreeId: WhichTree.Left
        })
      })
      it("redirects from 3 -> 4 ", async () => {
        let mergeMap = await mergeMaps(smallTree, bigTree, flipped)
        expect(goLeftAtDiffingPoint(mergeMap, 9)).toEqual({
          selected: 10,
          selectedTreeId: WhichTree.Left
        })
      })
    })
  })
})
