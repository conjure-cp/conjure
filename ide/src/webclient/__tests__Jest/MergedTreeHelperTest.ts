import Node, { WhichTree } from "../src/modules/Node"
import {
  loadAllDiffs,
  mergeMaps,
  getDescList,
  assignTreeIds
} from "../src/modules/ForestHelper"
// import { FetchMock} from "jest-fetch-mock"
import { cloneDeep } from "lodash"
import * as d3 from "d3"
import { node } from "prop-types"
import {
  goLeftAtDiffingPoint,
  reviseGoLeft,
  shouldBeRightTree,
  goLeftMerged,
  goUpMerged,
  goDownMerged,
  goRightMerged,
  collapseMerged,
  expandMerged
} from "../src/modules/MergedTreeHelper"
import { flipDiffLocations } from "../src/modules/Helper"

import { diffPoints as differAt0 } from "../__testResources__/differAt0"

import {
  coreOf3 as coreOf3Normal,
  coreOf7 as coreOf7Normal,
  coreOf17 as coreOf17Normal,
  coreOf21 as coreOf21Normal,
  coreOf27 as coreOf27Normal,
  descendantsOf3 as descendantsOf3Normal,
  descendantsOf17 as descendantsOf17Normal,
  descendantsOf28 as descendantsOf28Normal,
  // descendantsOf4 as descendantsOf4Normal,
  // descendantsOf7 as descendantsOf7Normal,
  // descendantsOf18 as descendantsOf18Normal,
  // descendantsOf21 as descendantsOf21Normal,
  // descendantsOf28 as descendantsOf28Normal,
  core as coreNormal8
} from "../__testResources__/normal-8"
import {
  coreOf3 as coreOf3Sacbounds,
  coreOf4 as coreOf4Sacbounds,
  coreOf6 as coreOf6Sacbounds,
  coreOf7 as coreOf7Sacbounds,
  coreOf9 as coreOf9Sacbounds,
  core as coreSacbounds8
} from "../__testResources__/sacbounds-8"

import { core as coreNoOpt } from "../__testResources__/noOpt-8"
import { core as coreNoOptSymmBreak } from "../__testResources__/noOptSymmBreak-8"

import { bigToSmall } from "../__testResources__/normalVSSacbounds-8"
import { makeState, insertNodesBoyo } from "../src/modules/TreeHelper"

const smallToBig = flipDiffLocations(bigToSmall)

async function loadTreeBigOnLeftSmallOnRight() {
  let bigTree: any
  let smallTree: any

  fetchMock.resetMocks()
  fetchMock
    .once(JSON.stringify(coreOf3Normal))
    .once(JSON.stringify(coreOf3Sacbounds))
    .once(JSON.stringify(coreOf7Normal))
    .once(JSON.stringify(coreOf4Sacbounds))
    .once(JSON.stringify(coreOf17Normal))
    .once(JSON.stringify(coreOf6Sacbounds))
    .once(JSON.stringify(coreOf21Normal))
    .once(JSON.stringify(coreOf7Sacbounds))
    .once(JSON.stringify(coreOf27Normal))
    .once(JSON.stringify(coreOf9Sacbounds))

    .once(JSON.stringify(coreOf3Normal))
    .once(JSON.stringify(coreOf3Sacbounds))
    .once(JSON.stringify(coreOf7Normal))
    .once(JSON.stringify(coreOf4Sacbounds))
    .once(JSON.stringify(coreOf17Normal))
    .once(JSON.stringify(coreOf6Sacbounds))
    .once(JSON.stringify(coreOf21Normal))
    .once(JSON.stringify(coreOf7Sacbounds))
    .once(JSON.stringify(coreOf27Normal))
    .once(JSON.stringify(coreOf9Sacbounds))

  let res = await loadAllDiffs(
    ["", "s"],
    [coreNormal8, coreSacbounds8],
    bigToSmall,
    5000
  )

  bigTree = res[0]

  bigTree = insertNodesBoyo(descendantsOf3Normal, bigTree, WhichTree.Both)
  bigTree = insertNodesBoyo(descendantsOf17Normal, bigTree, WhichTree.Both)
  bigTree = insertNodesBoyo(descendantsOf28Normal, bigTree, WhichTree.Both)

  smallTree = res[1]

  assignTreeIds(bigTree, smallTree, bigToSmall)
  return { smallTree, bigTree }
}
async function loadTreeSmallOnLeftBigOnRight() {
  fetchMock.resetMocks()
  fetchMock
    .once(JSON.stringify(coreOf3Sacbounds))
    .once(JSON.stringify(coreOf3Normal))
    .once(JSON.stringify(coreOf4Sacbounds))
    .once(JSON.stringify(coreOf7Normal))
    .once(JSON.stringify(coreOf6Sacbounds))
    .once(JSON.stringify(coreOf17Normal))
    .once(JSON.stringify(coreOf7Sacbounds))
    .once(JSON.stringify(coreOf21Normal))
    .once(JSON.stringify(coreOf9Sacbounds))
    .once(JSON.stringify(coreOf27Normal))

    .once(JSON.stringify(coreOf3Sacbounds))
    .once(JSON.stringify(coreOf3Normal))
    .once(JSON.stringify(coreOf4Sacbounds))
    .once(JSON.stringify(coreOf7Normal))
    .once(JSON.stringify(coreOf6Sacbounds))
    .once(JSON.stringify(coreOf17Normal))
    .once(JSON.stringify(coreOf7Sacbounds))
    .once(JSON.stringify(coreOf21Normal))
    .once(JSON.stringify(coreOf9Sacbounds))
    .once(JSON.stringify(coreOf27Normal))

  let res = await loadAllDiffs(
    ["", "s"],
    [coreSacbounds8, coreNormal8],
    smallToBig,
    5000
  )

  let bigTree = res[1]
  bigTree = insertNodesBoyo(descendantsOf3Normal, bigTree, WhichTree.Both)
  bigTree = insertNodesBoyo(descendantsOf17Normal, bigTree, WhichTree.Both)
  bigTree = insertNodesBoyo(descendantsOf28Normal, bigTree, WhichTree.Both)

  assignTreeIds(res[0], bigTree, smallToBig)

  return { smallTree: res[0], bigTree }
}

describe("suite to test MergedTreeHelper", () => {
  let bigTree: any
  let smallTree: any

  describe("test should be right tree", () => {
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

      it("should be true for going from 4 to 6", async () => {
        for (let i = 4; i < 7; i++) {
          expect(shouldBeRightTree(smallTree, bigTree, i, true)).toBeTruthy()
        }
      })

      it("should be false for going to 7", async () => {
        expect(shouldBeRightTree(smallTree, bigTree, 7, true)).toBeFalsy()
      })

      it("should be true for going from 8 to 15", async () => {
        for (let i = 8; i < 16; i++) {
          expect(shouldBeRightTree(smallTree, bigTree, i, true)).toBeTruthy()
        }
      })
      it("should be false for going to 16", async () => {
        expect(shouldBeRightTree(smallTree, bigTree, 16, true)).toBeFalsy()
      })
      it("should be true for going from 18 to 20", async () => {
        for (let i = 18; i < 21; i++) {
          expect(shouldBeRightTree(smallTree, bigTree, i, true)).toBeTruthy()
        }
      })
      it("should be false for going to 21", async () => {
        expect(shouldBeRightTree(smallTree, bigTree, 21, true)).toBeFalsy()
      })
      it("should be true for going from 22 to 25", async () => {
        for (let i = 22; i < 26; i++) {
          expect(shouldBeRightTree(smallTree, bigTree, i, true)).toBeTruthy()
        }
      })
      it("should be false for going to 26", async () => {
        expect(shouldBeRightTree(smallTree, bigTree, 26, true)).toBeFalsy()
      })
      it("should be true for going from 28 to 32", async () => {
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
  })

  describe("test revise goleft, it redirects to a node to the right of the next diff location", () => {
    let lMap
    let rMap
    let merged: Record<number, Node>

    describe("Left: small | Right: big", () => {
      beforeEach(async () => {
        let res = await loadTreeSmallOnLeftBigOnRight()
        bigTree = res.bigTree
        smallTree = res.smallTree
        lMap = cloneDeep(smallTree)
        rMap = cloneDeep(bigTree)
        assignTreeIds(lMap, rMap, smallToBig)
        merged = mergeMaps(lMap, rMap, smallToBig)
      })

      it("redirects 15 -> 4 ", async () => {
        let res = reviseGoLeft(merged, 15, WhichTree.Right, smallToBig)
        expect(res).toEqual({ selected: 4, treeId: WhichTree.Right })
      })

      it("redirects 6 -> 5 ", async () => {
        let res = reviseGoLeft(merged, 6, WhichTree.Right, smallToBig)
        expect(res).toEqual({ selected: 5, treeId: WhichTree.Both })
      })

      it("redirects 25 -> 18 ", async () => {
        let res = reviseGoLeft(merged, 25, WhichTree.Right, smallToBig)
        expect(res).toEqual({ selected: 18, treeId: WhichTree.Right })
      })

      it("redirects 20 -> 8 ", async () => {
        let res = reviseGoLeft(merged, 20, WhichTree.Right, smallToBig)
        expect(res).toEqual({ selected: 8, treeId: WhichTree.Both })
      })
    })

    describe("Left: big | right: small", () => {
      beforeEach(async () => {
        let res = await loadTreeBigOnLeftSmallOnRight()
        bigTree = res.bigTree
        smallTree = res.smallTree
        lMap = cloneDeep(bigTree)
        rMap = cloneDeep(smallTree)
        assignTreeIds(lMap, rMap, bigToSmall)
        merged = mergeMaps(lMap, rMap, bigToSmall)
      })

      // it("redirects 15 -> 16", async () => {
      //   let res = reviseGoLeft(merged, 15, WhichTree.Right, bigToSmall)
      //   expect(res).toEqual({ selected: 16, treeId: WhichTree.Both })
      // })

      // it("redirects 7 -> 26", async () => {
      //   let res = reviseGoLeft(merged, 7, WhichTree.Right, bigToSmall)
      //   expect(res).toEqual({ selected: 26, treeId: WhichTree.Both })
      // })

      // it("redirects 10 ->  10", async () => {
      //   let res = reviseGoLeft(merged, 10, WhichTree.Right, bigToSmall)
      //   expect(res).toEqual({ selected: 10, treeId: WhichTree.Right })
      // })
    })
  })

  describe("test go left at diffing point", () => {
    describe("Left: small | Right: big", () => {
      beforeEach(async () => {
        bigTree = makeState(coreNormal8, 0).id2Node
        smallTree = makeState(coreSacbounds8, 0).id2Node
      })
    })
    it("redirects from 3 -> 4 on the left tree", async () => {
      // await noDiffsSmallBig()

      let res = await goLeftAtDiffingPoint(
        smallTree,
        bigTree,
        3,
        smallToBig,
        "",
        "",
        5000
      )
      expect(res.selected).toEqual(4)
      expect(res.selectedTreeId).toEqual(WhichTree.Both)
    })
    it("redirects from 6 -> 7 on the left tree", async () => {
      // await noDiffsSmallBig()

      let res = await goLeftAtDiffingPoint(
        smallTree,
        bigTree,
        6,
        smallToBig,
        "",
        "",
        5000
      )
      expect(res.selected).toEqual(7)
      expect(res.selectedTreeId).toEqual(WhichTree.Both)
    })
    it("redirects from 9 -> 10 on the left tree", async () => {
      let res = await goLeftAtDiffingPoint(
        smallTree,
        bigTree,
        9,
        smallToBig,
        "",
        "",
        5000
      )
      expect(res.selected).toEqual(10)
      expect(res.selectedTreeId).toEqual(WhichTree.Left)
    })

    describe("Left: Big | Right: small", () => {
      beforeEach(async () => {
        // bigTree = makeState(coreNormal8, 0).id2Node
        // smallTree = makeState(coreSacbounds8, 0).id2Node

        let res = await loadTreeBigOnLeftSmallOnRight()
        bigTree = res.bigTree
        smallTree = res.smallTree
      })
      it("redirects from 3 -> 4 on the left tree", async () => {
        // await noDiffsBigSmall()

        let res = await goLeftAtDiffingPoint(
          bigTree,
          smallTree,
          3,
          bigToSmall,
          "",
          "",
          5000
        )
        expect(res.selected).toEqual(4)
        expect(res.selectedTreeId).toEqual(WhichTree.Left)
      })
      it("redirects from 17 -> 18 on the left tree", async () => {
        let res = await goLeftAtDiffingPoint(
          bigTree,
          smallTree,
          17,
          bigToSmall,
          "",
          "",
          5000
        )
        expect(res.selected).toEqual(18)
        expect(res.selectedTreeId).toEqual(WhichTree.Left)
      })

      it("redirects from 27 -> 28 on the left tree", async () => {
        let res = await goLeftAtDiffingPoint(
          bigTree,
          smallTree,
          27,
          bigToSmall,
          "",
          "",
          5000
        )
        expect(res.selected).toEqual(28)
        expect(res.selectedTreeId).toEqual(WhichTree.Left)
      })
    })
  })

  describe("test go left", () => {
    describe("Left: small | Right: big", () => {
      beforeEach(async () => {
        let res = await loadTreeSmallOnLeftBigOnRight()
        bigTree = res.bigTree
        smallTree = res.smallTree
      })

      it(" 0 -> 1 ", async () => {
        let res = await goLeftMerged(
          0,
          WhichTree.Both,
          "",
          "",
          smallTree,
          bigTree,
          smallToBig,
          5000
        )
        expect(res.selected).toEqual(1)
        expect(res.selectedTreeId).toEqual(WhichTree.Both)
      })

      it(" 1 -> 2 ", async () => {
        let res = await goLeftMerged(
          1,
          WhichTree.Both,
          "",
          "",
          smallTree,
          bigTree,
          smallToBig,
          5000
        )
        expect(res.selected).toEqual(2)
        expect(res.selectedTreeId).toEqual(WhichTree.Both)
      })

      it(" 2 -> 3 ", async () => {
        let res = await goLeftMerged(
          2,
          WhichTree.Both,
          "",
          "",
          smallTree,
          bigTree,
          smallToBig,
          5000
        )
        expect(res.selected).toEqual(3)
        expect(res.selectedTreeId).toEqual(WhichTree.Both)
      })

      it(" 3 -> 4 left ", async () => {
        let res = await goLeftMerged(
          3,
          WhichTree.Both,
          "",
          "",
          smallTree,
          bigTree,
          smallToBig,
          5000
        )
        expect(res.selected).toEqual(4)
        expect(res.selectedTreeId).toEqual(WhichTree.Both)
      })

      it(" 4 -> 8 Right ", async () => {
        let res = await goLeftMerged(
          4,
          WhichTree.Both,
          "",
          "",
          smallTree,
          bigTree,
          smallToBig,
          5000
        )
        expect(res.selected).toEqual(8)
        expect(res.selectedTreeId).toEqual(WhichTree.Right)
      })

      it(" 8 -> 9 ", async () => {
        let res = await goLeftMerged(
          8,
          WhichTree.Right,
          "",
          "",
          smallTree,
          bigTree,
          smallToBig,
          5000
        )
        expect(res.selected).toEqual(9)
        expect(res.selectedTreeId).toEqual(WhichTree.Right)
      })

      it(" 7 -> 22 Right ", async () => {
        let res = await goLeftMerged(
          7,
          WhichTree.Both,
          "",
          "",
          smallTree,
          bigTree,
          smallToBig,
          5000
        )
        expect(res.selected).toEqual(22)
        expect(res.selectedTreeId).toEqual(WhichTree.Right)
      })

      it(" 6 Left -> 7", async () => {
        let res = await goLeftMerged(
          6,
          WhichTree.Both,
          "",
          "",
          smallTree,
          bigTree,
          smallToBig,
          5000
        )
        expect(res.selected).toEqual(7)
        expect(res.selectedTreeId).toEqual(WhichTree.Both)
      })

      it(" 8 -> 9", async () => {
        let res = await goLeftMerged(
          8,
          WhichTree.Both,
          "",
          "",
          smallTree,
          bigTree,
          smallToBig,
          5000
        )
        expect(res.selected).toEqual(9)
        expect(res.selectedTreeId).toEqual(WhichTree.Both)
      })

      it(" 9 -> 10", async () => {
        let res = await goLeftMerged(
          9,
          WhichTree.Both,
          "",
          "",
          smallTree,
          bigTree,
          smallToBig,
          5000
        )
        expect(res.selected).toEqual(10)
        expect(res.selectedTreeId).toEqual(WhichTree.Left)
      })
    })

    describe("Left: big | Right: small", () => {
      beforeEach(async () => {
        let res = await loadTreeBigOnLeftSmallOnRight()
        bigTree = res.bigTree
        smallTree = res.smallTree
        // console.log(bigTree)
      })

      describe("Left tree only", () => {
        it(" 0 -> 1 ", async () => {
          let res = await goLeftMerged(
            0,
            WhichTree.Both,
            "",
            "",
            bigTree,
            smallTree,
            bigToSmall,
            5000
          )
          expect(res.selected).toEqual(1)
          expect(res.selectedTreeId).toEqual(WhichTree.Both)
        })

        it(" 1 -> 2 ", async () => {
          let res = await goLeftMerged(
            1,
            WhichTree.Both,
            "",
            "",
            bigTree,
            smallTree,
            bigToSmall,
            5000
          )
          expect(res.selected).toEqual(2)
          expect(res.selectedTreeId).toEqual(WhichTree.Both)
        })
        it(" 2 -> 3 ", async () => {
          let res = await goLeftMerged(
            2,
            WhichTree.Both,
            "",
            "",
            bigTree,
            smallTree,
            bigToSmall,
            5000
          )
          expect(res.selected).toEqual(3)
          expect(res.selectedTreeId).toEqual(WhichTree.Both)
        })

        it(" 3 -> 4 Left ", async () => {
          let res = await goLeftMerged(
            3,
            WhichTree.Both,
            "",
            "",
            bigTree,
            smallTree,
            bigToSmall,
            5000
          )
          expect(res.selected).toEqual(4)
          expect(res.selectedTreeId).toEqual(WhichTree.Left)
        })

        it(" 6 Left -> 7 Both ", async () => {
          let res = await goLeftMerged(
            6,
            WhichTree.Left,
            "",
            "",
            bigTree,
            smallTree,
            bigToSmall,
            5000
          )
          expect(res.selected).toEqual(7)
          expect(res.selectedTreeId).toEqual(WhichTree.Both)
        })

        it(" 15 Left ->  16", async () => {
          let res = await goLeftMerged(
            15,
            WhichTree.Left,
            "",
            "",
            bigTree,
            smallTree,
            bigToSmall,
            5000
          )
          expect(res.selected).toEqual(16)
          expect(res.selectedTreeId).toEqual(WhichTree.Both)
        })

        it(" 25 Left ->  26", async () => {
          let res = await goLeftMerged(
            25,
            WhichTree.Left,
            "",
            "",
            bigTree,
            smallTree,
            bigToSmall,
            5000
          )
          expect(res.selected).toEqual(26)
          expect(res.selectedTreeId).toEqual(WhichTree.Both)
        })
        
        it(" 29 Left -> 30", async () => {
          let res = await goLeftMerged(
            29,
            WhichTree.Left,
            "",
            "",
            bigTree,
            smallTree,
            bigToSmall,
            5000
          )
          expect(res.selected).toEqual(30)
          expect(res.selectedTreeId).toEqual(WhichTree.Left)
        })
      })
    })
  })

  describe("test go up", () => {
    describe("Left: small | Right: big", () => {
      beforeEach(async () => {
        let res = await loadTreeSmallOnLeftBigOnRight()
        bigTree = res.bigTree
        smallTree = res.smallTree
      })
      it("4 -> 3", async () => {
        expect(
          goUpMerged(smallTree, bigTree, 4, WhichTree.Both, smallToBig)
        ).toEqual({ selected: 3, selectedTreeId: WhichTree.Both })
      })
      it("3 -> 2", async () => {
        expect(
          goUpMerged(smallTree, bigTree, 3, WhichTree.Both, smallToBig)
        ).toEqual({ selected: 2, selectedTreeId: WhichTree.Both })
      })
      it("2 -> 1", async () => {
        expect(
          goUpMerged(smallTree, bigTree, 2, WhichTree.Both, smallToBig)
        ).toEqual({ selected: 1, selectedTreeId: WhichTree.Both })
      })
      it("1 -> 0", async () => {
        expect(
          goUpMerged(smallTree, bigTree, 1, WhichTree.Both, smallToBig)
        ).toEqual({ selected: 0, selectedTreeId: WhichTree.Both })
      })

      it("6 Right -> 5 Right", async () => {
        expect(
          goUpMerged(smallTree, bigTree, 6, WhichTree.Right, smallToBig)
        ).toEqual({ selected: 5, selectedTreeId: WhichTree.Right })
      })

      it("4 Right -> 3", async () => {
        expect(
          goUpMerged(smallTree, bigTree, 4, WhichTree.Right, smallToBig)
        ).toEqual({ selected: 3, selectedTreeId: WhichTree.Both })
      })
      it("7 Right -> 3", async () => {
        expect(
          goUpMerged(smallTree, bigTree, 7, WhichTree.Right, smallToBig)
        ).toEqual({ selected: 3, selectedTreeId: WhichTree.Both })
      })
      it("30 Right -> 9", async () => {
        expect(
          goUpMerged(smallTree, bigTree, 30, WhichTree.Right, smallToBig)
        ).toEqual({ selected: 9, selectedTreeId: WhichTree.Both })
      })
    })

    describe("Left: big | Right: small", () => {
      beforeEach(async () => {
        let res = await loadTreeBigOnLeftSmallOnRight()
        bigTree = res.bigTree
        smallTree = res.smallTree
      })
      it("6 Left -> 5 Left", async () => {
        expect(
          goUpMerged(bigTree, smallTree, 6, WhichTree.Left, bigToSmall)
        ).toEqual({ selected: 5, selectedTreeId: WhichTree.Left })
      })

      it("4 Left -> 3 ", async () => {
        expect(
          goUpMerged(bigTree, smallTree, 4, WhichTree.Left, bigToSmall)
        ).toEqual({ selected: 3, selectedTreeId: WhichTree.Both })
      })

      it("4 Right -> 3 ", async () => {
        expect(
          goUpMerged(bigTree, smallTree, 4, WhichTree.Right, bigToSmall)
        ).toEqual({ selected: 3, selectedTreeId: WhichTree.Both })
      })
    })

    describe("test go down", () => {
      it("Must be able to go down when the node 0 is the only Both node ", async () => {
        let leftMap = makeState(coreNoOpt, 0).id2Node
        let rightMap = makeState(coreNoOptSymmBreak, 0).id2Node

        assignTreeIds(leftMap, rightMap, differAt0)

        let res = await goDownMerged(
          leftMap,
          rightMap,
          0,
          WhichTree.Both,
          differAt0,
          "",
          "",
          5000
        )

        expect(res.selected).toBe(1)
        expect(res.selectedTreeId).toBe(WhichTree.Left)
      })

      describe("Left: small | Right: big", () => {
        beforeEach(async () => {
          let res = await loadTreeSmallOnLeftBigOnRight()
          bigTree = res.bigTree
          smallTree = res.smallTree
        })
        it("3 -> 4 Right ", async () => {
          let res = await goDownMerged(
            smallTree,
            bigTree,
            3,
            WhichTree.Both,
            smallToBig,
            "",
            "",
            5000
          )
          expect(res.selected).toEqual(4)
          expect(res.selectedTreeId).toEqual(WhichTree.Both)
        })

        it("4 Right -> 5 Right ", async () => {
          let res = await goDownMerged(
            smallTree,
            bigTree,
            4,
            WhichTree.Right,
            smallToBig,
            "",
            "",
            5000
          )
          expect(res.selected).toEqual(5)
          expect(res.selectedTreeId).toEqual(WhichTree.Right)
        })

        it("4 -> 8 Right ", async () => {
          let res = await goDownMerged(
            smallTree,
            bigTree,
            4,
            WhichTree.Both,
            smallToBig,
            "",
            "",
            5000
          )
          expect(res.selected).toEqual(8)
          expect(res.selectedTreeId).toEqual(WhichTree.Right)
        })

        it("6 Right -> 7 Right ", async () => {
          let res = await goDownMerged(
            smallTree,
            bigTree,
            6,
            WhichTree.Right,
            smallToBig,
            "",
            "",
            5000
          )
          expect(res.selected).toEqual(5)
          expect(res.selectedTreeId).toEqual(WhichTree.Both)
        })

        it("9 -> 28 Right ", async () => {
          let res = await goDownMerged(
            smallTree,
            bigTree,
            9,
            WhichTree.Both,
            smallToBig,
            "",
            "",
            5000
          )
          expect(res.selected).toEqual(28)
          expect(res.selectedTreeId).toEqual(WhichTree.Right)
        })
      })

      describe("Left: big | Right: small", () => {
        beforeEach(async () => {
          let res = await loadTreeBigOnLeftSmallOnRight()
          bigTree = res.bigTree
          smallTree = res.smallTree
        })
        it("3 -> 4 left ", async () => {
          let res = await goDownMerged(
            bigTree,
            smallTree,
            3,
            WhichTree.Both,
            bigToSmall,
            "",
            "",
            5000
          )
          expect(res.selected).toEqual(4)
          expect(res.selectedTreeId).toEqual(WhichTree.Left)
        })
        it("17 -> 18 left ", async () => {
          let res = await goDownMerged(
            bigTree,
            smallTree,
            17,
            WhichTree.Both,
            bigToSmall,
            "",
            "",
            5000
          )
          expect(res.selected).toEqual(18)
          expect(res.selectedTreeId).toEqual(WhichTree.Left)
        })
        it("15 left -> 16 ", async () => {
          let res = await goDownMerged(
            bigTree,
            smallTree,
            15,
            WhichTree.Left,
            bigToSmall,
            "",
            "",
            5000
          )
          expect(res.selected).toEqual(16)
          expect(res.selectedTreeId).toEqual(WhichTree.Both)
        })
        it("31 left -> 32 left ", async () => {
          let res = await goDownMerged(
            bigTree,
            smallTree,
            31,
            WhichTree.Left,
            bigToSmall,
            "",
            "",
            5000
          )
          expect(res.selected).toEqual(32)
          expect(res.selectedTreeId).toEqual(WhichTree.Left)
        })
      })
    })

    describe("test go right", () => {
      it("Must be able to go right when the node 0 is the only Both node ", async () => {
        let leftMap = makeState(coreNoOpt, 0).id2Node
        let rightMap = makeState(coreNoOptSymmBreak, 0).id2Node

        assignTreeIds(leftMap, rightMap, differAt0)

        let res = await goRightMerged(
          leftMap,
          rightMap,
          0,
          WhichTree.Both,
          differAt0
        )

        expect(res.selected).toBe(1)
        expect(res.selectedTreeId).toBe(WhichTree.Right)
      })

      describe("Left: small | Right: big", () => {
        beforeEach(async () => {
          let res = await loadTreeSmallOnLeftBigOnRight()
          bigTree = res.bigTree
          smallTree = res.smallTree
        })

        it("2 -> 5 ", async () => {
          let res = await goRightMerged(
            smallTree,
            bigTree,
            2,
            WhichTree.Both,
            smallToBig
          )
          expect(res.selected).toEqual(5)
          expect(res.selectedTreeId).toEqual(WhichTree.Both)
        })

        it("3 -> 4 Right ", async () => {
          let res = await goRightMerged(
            smallTree,
            bigTree,
            3,
            WhichTree.Both,
            smallToBig
          )
          expect(res.selected).toEqual(4)
          expect(res.selectedTreeId).toEqual(WhichTree.Right)
        })
        it("6 -> 21 Right ", async () => {
          let res = await goRightMerged(
            smallTree,
            bigTree,
            6,
            WhichTree.Both,
            smallToBig
          )
          expect(res.selected).toEqual(18)
          expect(res.selectedTreeId).toEqual(WhichTree.Right)
        })
        it("9 -> 30 Right ", async () => {
          let res = await goRightMerged(
            smallTree,
            bigTree,
            9,
            WhichTree.Both,
            smallToBig
          )
          expect(res.selected).toEqual(30)
          expect(res.selectedTreeId).toEqual(WhichTree.Right)
        })

        it("15 right -> 15 Right ", async () => {
          let res = await goRightMerged(
            smallTree,
            bigTree,
            15,
            WhichTree.Right,
            smallToBig
          )
          expect(res.selected).toEqual(15)
          expect(res.selectedTreeId).toEqual(WhichTree.Right)
        })

        describe("Left: big | Right: small", () => {
          beforeEach(async () => {
            let res = await loadTreeBigOnLeftSmallOnRight()
            bigTree = res.bigTree
            smallTree = res.smallTree
          })
          it("3 -> 7 ", async () => {
            let res = await goRightMerged(
              bigTree,
              smallTree,
              3,
              WhichTree.Both,
              bigToSmall
            )
            expect(res.selected).toEqual(7)
            expect(res.selectedTreeId).toEqual(WhichTree.Both)
          })
          it("17 -> 21 Right ", async () => {
            let res = await goRightMerged(
              bigTree,
              smallTree,
              17,
              WhichTree.Both,
              bigToSmall
            )
            expect(res.selected).toEqual(21)
            expect(res.selectedTreeId).toEqual(WhichTree.Both)
          })
          it("27 -> 10 Right ", async () => {
            let res = await goRightMerged(
              bigTree,
              smallTree,
              27,
              WhichTree.Both,
              bigToSmall
            )
            expect(res.selected).toEqual(10)
            expect(res.selectedTreeId).toEqual(WhichTree.Right)
          })

          it("10 Right -> 10 Right ", async () => {
            let res = await goRightMerged(
              bigTree,
              smallTree,
              10,
              WhichTree.Both,
              bigToSmall
            )
            expect(res.selected).toEqual(10)
            expect(res.selectedTreeId).toEqual(WhichTree.Both)
          })
        })

        describe("test collapse", () => {
          let lMap
          let rMap

          describe("Left: small | Right: big", () => {
            beforeEach(async () => {
              let res = await loadTreeSmallOnLeftBigOnRight()
              bigTree = res.bigTree
              smallTree = res.smallTree
              lMap = cloneDeep(smallTree)
              rMap = cloneDeep(bigTree)
              assignTreeIds(lMap, rMap, smallToBig)
            })
          })

          it("Collapsing 2", async () => {
            let res = collapseMerged(
              smallTree,
              bigTree,
              2,
              WhichTree.Both,
              smallToBig
            )
            let merged = mergeMaps(res.leftMap, res.rightMap, smallToBig)
            expect(merged[2].children).toBeUndefined()
          })

          it("Collapsing 3 ", async () => {
            let res = collapseMerged(
              smallTree,
              bigTree,
              3,
              WhichTree.Both,
              smallToBig
            )
            let merged = mergeMaps(res.leftMap, res.rightMap, smallToBig)
            expect(merged[3].children).toBeUndefined()
          })

          it("Collapsing 4 ", async () => {
            let res = collapseMerged(
              smallTree,
              bigTree,
              4,
              WhichTree.Both,
              smallToBig
            )
            let merged = mergeMaps(res.leftMap, res.rightMap, smallToBig)
            expect(merged[4].children).toBeUndefined()
          })

          it("Collapsing 8 Right", async () => {
            let res = collapseMerged(
              smallTree,
              bigTree,
              8,
              WhichTree.Right,
              smallToBig
            )
            expect(res.rightMap[8].children).toBeUndefined()
          })

          describe("Left: Big | Right: small", () => {
            beforeEach(async () => {
              let res = await loadTreeBigOnLeftSmallOnRight()
              bigTree = res.bigTree
              smallTree = res.smallTree
              assignTreeIds(bigTree, smallTree, smallToBig)
            })
          })

          it("Collapsing 2", async () => {
            let res = collapseMerged(
              smallTree,
              bigTree,
              2,
              WhichTree.Both,
              smallToBig
            )
            let merged = mergeMaps(res.leftMap, res.rightMap, smallToBig)
            expect(merged[2].children).toBeUndefined()
          })

          it("Collapsing 3 ", async () => {
            let res = collapseMerged(
              smallTree,
              bigTree,
              3,
              WhichTree.Both,
              smallToBig
            )
            let merged = mergeMaps(res.leftMap, res.rightMap, smallToBig)
            expect(merged[3].children).toBeUndefined()
          })

          it("Collapsing 4 ", async () => {
            let res = collapseMerged(
              smallTree,
              bigTree,
              4,
              WhichTree.Both,
              smallToBig
            )
            let merged = mergeMaps(res.leftMap, res.rightMap, smallToBig)
            expect(merged[4].children).toBeUndefined()
          })

          it("Collapsing 4 Left ", async () => {
            let res = collapseMerged(
              smallTree,
              bigTree,
              4,
              WhichTree.Left,
              smallToBig
            )
            let merged = mergeMaps(res.leftMap, res.rightMap, smallToBig)
            expect(merged[4].children).toBeUndefined()
          })
        })

        describe("test expand", () => {
          let lMap
          let rMap

          describe("Left: small | Right: big", () => {
            beforeEach(async () => {
              let res = await loadTreeSmallOnLeftBigOnRight()
              bigTree = res.bigTree
              smallTree = res.smallTree
              lMap = cloneDeep(smallTree)
              rMap = cloneDeep(bigTree)
              assignTreeIds(lMap, rMap, smallToBig)
            })
          })

          it("expanding 2", async () => {
            let oldMerged = mergeMaps(smallTree, bigTree, smallToBig)

            let res1 = collapseMerged(
              smallTree,
              bigTree,
              2,
              WhichTree.Both,
              smallToBig
            )

            let res2 = expandMerged(
              res1.leftMap,
              res1.rightMap,
              2,
              WhichTree.Both,
              smallToBig
            )
            let merged = mergeMaps(res2.leftMap, res2.rightMap, smallToBig)

            expect(merged[2].children).toEqual(oldMerged[2].children)
          })

          it("Collapsing 3 ", async () => {
            let oldMerged = mergeMaps(smallTree, bigTree, smallToBig)

            let res1 = collapseMerged(
              smallTree,
              bigTree,
              3,
              WhichTree.Both,
              smallToBig
            )

            let res2 = expandMerged(
              res1.leftMap,
              res1.rightMap,
              3,
              WhichTree.Both,
              smallToBig
            )
            let merged = mergeMaps(res2.leftMap, res2.rightMap, smallToBig)

            expect(merged[3].children).toEqual(oldMerged[3].children)
          })

          it("Collapsing 4 ", async () => {
            let oldMerged = mergeMaps(smallTree, bigTree, smallToBig)

            let res1 = collapseMerged(
              smallTree,
              bigTree,
              4,
              WhichTree.Both,
              smallToBig
            )

            let res2 = expandMerged(
              res1.leftMap,
              res1.rightMap,
              4,
              WhichTree.Both,
              smallToBig
            )
            let merged = mergeMaps(res2.leftMap, res2.rightMap, smallToBig)

            expect(merged[4].children).toEqual(oldMerged[4].children)
          })

          it("Collapsing 8 Right", async () => {
            let res1 = collapseMerged(
              smallTree,
              bigTree,
              8,
              WhichTree.Both,
              smallToBig
            )

            let res2 = expandMerged(
              res1.leftMap,
              res1.rightMap,
              8,
              WhichTree.Both,
              smallToBig
            )

            expect(res2.rightMap[8].children).toEqual(bigTree[8].children)
          })

          describe("Left: Big | Right: small", () => {
            beforeEach(async () => {
              let res = await loadTreeBigOnLeftSmallOnRight()
              bigTree = res.bigTree
              smallTree = res.smallTree
              assignTreeIds(bigTree, smallTree, smallToBig)
            })
          })

          it("expanding 2", async () => {
            let oldMerged = mergeMaps(bigTree, smallTree, smallToBig)

            let res1 = collapseMerged(
              bigTree,
              smallTree,
              2,
              WhichTree.Both,
              smallToBig
            )

            let res2 = expandMerged(
              res1.leftMap,
              res1.rightMap,
              2,
              WhichTree.Both,
              smallToBig
            )
            let merged = mergeMaps(res2.leftMap, res2.rightMap, smallToBig)

            expect(merged[2].children).toEqual(oldMerged[2].children)
          })

          it("Collapsing 3 ", async () => {
            let oldMerged = mergeMaps(bigTree, smallTree, smallToBig)

            let res1 = collapseMerged(
              bigTree,
              smallTree,
              3,
              WhichTree.Both,
              smallToBig
            )

            let res2 = expandMerged(
              res1.leftMap,
              res1.rightMap,
              3,
              WhichTree.Both,
              smallToBig
            )
            let merged = mergeMaps(res2.leftMap, res2.rightMap, smallToBig)

            expect(merged[3].children).toEqual(oldMerged[3].children)
          })

          it("Collapsing 4 ", async () => {
            let res = collapseMerged(
              smallTree,
              bigTree,
              4,
              WhichTree.Both,
              smallToBig
            )
            let merged = mergeMaps(res.leftMap, res.rightMap, smallToBig)
            expect(merged[4].children).toBeUndefined()
          })

          it("expanding 4 Left ", async () => {
            let res1 = collapseMerged(
              smallTree,
              bigTree,
              4,
              WhichTree.Both,
              smallToBig
            )

            let res2 = expandMerged(
              res1.leftMap,
              res1.rightMap,
              4,
              WhichTree.Both,
              smallToBig
            )

            expect(res2.rightMap[4].children).toEqual(bigTree[4].children)
          })
        })
      })
    })
  })
})
