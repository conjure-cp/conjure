import Node, { WhichTree } from "../src/modules/Node"
import {
  loadAllDiffs,
  mergeMaps,
  getDescList
} from "../src/modules/ForestHelper"
import { fetchAncestors, goLeftBoyo } from "../src/modules/MovementHelper"
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
  goRightMerged
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
import { bigToSmall } from "./resources/normalVSSacbounds-8"
import { makeState } from "../src/modules/TreeHelper"

const smallToBig = flipDiffLocations(bigToSmall)

async function loadTreeBigOnLeftSmallOnRight() {
  let bigTree: any
  let smallTree: any

  fetchMock.resetMocks()
  fetchMock
    .once(JSON.stringify(coreOf3Normal))
    .once(JSON.stringify(coreOf3Sacbounds))
    .once(JSON.stringify(coreOf17Normal))
    .once(JSON.stringify(coreOf6Sacbounds))
    .once(JSON.stringify(coreOf27Normal))
    .once(JSON.stringify(coreOf9Sacbounds))
    .once(JSON.stringify(descendantsOf4Normal))
    .once(JSON.stringify(descendantsOf7Normal))
    .once(JSON.stringify(descendantsOf18Normal))
    .once(JSON.stringify(descendantsOf21Normal))
    .once(JSON.stringify(descendantsOf28Normal))
    .once(JSON.stringify([]))

  let res = await loadAllDiffs(
    ["", "s"],
    [coreNormal, coreSacbounds],
    bigToSmall,
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
    .once(JSON.stringify(coreOf3Normal))
    .once(JSON.stringify(coreOf6Sacbounds))
    .once(JSON.stringify(coreOf17Normal))
    .once(JSON.stringify(coreOf9Sacbounds))
    .once(JSON.stringify(coreOf27Normal))

    .once(JSON.stringify(descendantsOf4Normal))
    .once(JSON.stringify(descendantsOf7Normal))
    .once(JSON.stringify(descendantsOf18Normal))
    .once(JSON.stringify(descendantsOf21Normal))
    .once(JSON.stringify(descendantsOf28Normal))
    .once(JSON.stringify([]))

  let res = await loadAllDiffs(
    ["", "s"],
    [coreSacbounds, coreNormal],
    smallToBig,
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

// async function noDiffsBigSmall() {
//   fetchMock.resetMocks()
//   fetchMock
//     .once(JSON.stringify(coreOf3Normal))
//     .once(JSON.stringify(coreOf3Sacbounds))
//     .once(JSON.stringify(coreOf17Normal))
//     .once(JSON.stringify(coreOf6Sacbounds))
//     .once(JSON.stringify(coreOf27Normal))
//     .once(JSON.stringify(coreOf9Sacbounds))

//   return {
//     smallTree: makeState(coreSacbounds, 0).id2Node,
//     bigTree: makeState(coreNormal, 0).id2Node
//   }
// }

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
      const merged = mergeMaps(smallTree, bigTree, smallToBig)
      let res = reviseGoLeft(merged, 4, WhichTree.Left, smallToBig)
      expect(res).toEqual({ selected: 5, treeId: WhichTree.Both })
    })

    it("redirects 7 -> 8 from the left tree", async () => {
      const merged = mergeMaps(smallTree, bigTree, smallToBig)
      let res = reviseGoLeft(merged, 7, WhichTree.Left, smallToBig)
      expect(res).toEqual({ selected: 8, treeId: WhichTree.Both })
    })

    it("redirects 10 -> 10 from the left tree", async () => {
      const merged = mergeMaps(smallTree, bigTree, smallToBig)
      let res = reviseGoLeft(merged, 10, WhichTree.Both, smallToBig)
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
      const merged = mergeMaps(bigTree, smallTree, bigToSmall)
      let res = reviseGoLeft(merged, 4, WhichTree.Right, bigToSmall)
      expect(res).toEqual({ selected: 16, treeId: WhichTree.Both })
    })

    it("redirects 7 -> 26", async () => {
      const merged = mergeMaps(bigTree, smallTree, bigToSmall)
      let res = reviseGoLeft(merged, 7, WhichTree.Right, bigToSmall)
      expect(res).toEqual({ selected: 26, treeId: WhichTree.Both })
    })

    it("redirects 10 ->  10", async () => {
      const merged = mergeMaps(bigTree, smallTree, bigToSmall)
      let res = reviseGoLeft(merged, 10, WhichTree.Right, bigToSmall)
      expect(res).toEqual({ selected: 10, treeId: WhichTree.Right })
    })
  })

  describe("test go left at diffing point", () => {
    describe("Left: small | Right: big", () => {
      beforeEach(async () => {
        bigTree = makeState(coreNormal, 0).id2Node
        smallTree = makeState(coreSacbounds, 0).id2Node
      })
    })
    it("redirects from 3 -> 4 on the left tree", async () => {
      // await noDiffsSmallBig()
      fetchMock.resetMocks()
      fetchMock
        .once(JSON.stringify(coreOf3Sacbounds))
        .once(JSON.stringify(coreOf3Normal))

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
      expect(res.selectedTreeId).toEqual(WhichTree.Left)
    })
    it("redirects from 6 -> 7 on the left tree", async () => {
      // await noDiffsSmallBig()
      fetchMock.resetMocks()
      fetchMock
        .once(JSON.stringify(coreOf6Sacbounds))
        .once(JSON.stringify(coreOf17Normal))

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
      expect(res.selectedTreeId).toEqual(WhichTree.Left)
    })
    it("redirects from 9 -> 10 on the left tree", async () => {
      // await noDiffsSmallBig()
      fetchMock.resetMocks()
      fetchMock
        .once(JSON.stringify(coreOf9Sacbounds))
        .once(JSON.stringify(coreOf27Normal))

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
        bigTree = makeState(coreNormal, 0).id2Node
        smallTree = makeState(coreSacbounds, 0).id2Node
      })
      it("redirects from 3 -> 4 on the left tree", async () => {
        // await noDiffsBigSmall()
        fetchMock.resetMocks()
        fetchMock
          .once(JSON.stringify(coreOf3Normal))
          .once(JSON.stringify(coreOf3Sacbounds))

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
        fetchMock.resetMocks()
        fetchMock
          .once(JSON.stringify(coreOf17Normal))
          .once(JSON.stringify(coreOf6Sacbounds))
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
        fetchMock.resetMocks()
        fetchMock
          .once(JSON.stringify(coreOf27Normal))
          .once(JSON.stringify(coreOf9Sacbounds))

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
      describe("Left tree only", () => {
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
          fetchMock.resetMocks()
          fetchMock
            .once(JSON.stringify(coreOf3Sacbounds))
            .once(JSON.stringify(coreOf3Normal))

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
          expect(res.selectedTreeId).toEqual(WhichTree.Left)
        })

        it(" 4 Left -> 5 ", async () => {
          let res = await goLeftMerged(
            4,
            WhichTree.Left,
            "",
            "",
            smallTree,
            bigTree,
            smallToBig,
            5000
          )
          expect(res.selected).toEqual(5)
          expect(res.selectedTreeId).toEqual(WhichTree.Both)
        })

        it(" 5 -> 6 ", async () => {
          let res = await goLeftMerged(
            5,
            WhichTree.Both,
            "",
            "",
            smallTree,
            bigTree,
            smallToBig,
            5000
          )
          expect(res.selected).toEqual(6)
          expect(res.selectedTreeId).toEqual(WhichTree.Both)
        })

        it(" 6 -> 7 Left ", async () => {
          fetchMock.resetMocks()
          fetchMock
            .once(JSON.stringify(coreOf6Sacbounds))
            .once(JSON.stringify(coreOf17Normal))
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
          expect(res.selectedTreeId).toEqual(WhichTree.Left)
        })

        it(" 7 Left -> 8", async () => {
          let res = await goLeftMerged(
            7,
            WhichTree.Left,
            "",
            "",
            smallTree,
            bigTree,
            smallToBig,
            5000
          )
          expect(res.selected).toEqual(8)
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
          fetchMock.resetMocks()
          fetchMock
            .once(JSON.stringify(coreOf9Sacbounds))
            .once(JSON.stringify(coreOf27Normal))
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

      describe("Right tree only", () => {
        it(" 4 Right -> 5 Right", async () => {
          let res = await goLeftMerged(
            4,
            WhichTree.Right,
            "",
            "",
            smallTree,
            bigTree,
            smallToBig,
            5000
          )
          expect(res.selected).toEqual(5)
          expect(res.selectedTreeId).toEqual(WhichTree.Right)
        })

        it(" 5 Right -> 6 Right", async () => {
          let res = await goLeftMerged(
            5,
            WhichTree.Right,
            "",
            "",
            smallTree,
            bigTree,
            smallToBig,
            5000
          )
          expect(res.selected).toEqual(6)
          expect(res.selectedTreeId).toEqual(WhichTree.Right)
        })

        it("14 Right -> 15 Right", async () => {
          let res = await goLeftMerged(
            14,
            WhichTree.Right,
            "",
            "",
            smallTree,
            bigTree,
            smallToBig,
            5000
          )
          expect(res.selected).toEqual(15)
          expect(res.selectedTreeId).toEqual(WhichTree.Right)
        })

        it("15 Right -> 5 ", async () => {
          let res = await goLeftMerged(
            15,
            WhichTree.Right,
            "",
            "",
            smallTree,
            bigTree,
            smallToBig,
            5000
          )
          expect(res.selected).toEqual(5)
          expect(res.selectedTreeId).toEqual(WhichTree.Both)
        })
        it("20 Right -> 21 right", async () => {
          let res = await goLeftMerged(
            20,
            WhichTree.Right,
            "",
            "",
            smallTree,
            bigTree,
            smallToBig,
            5000
          )
          expect(res.selected).toEqual(21)
          expect(res.selectedTreeId).toEqual(WhichTree.Right)
        })

        it("25 Right -> 8", async () => {
          let res = await goLeftMerged(
            25,
            WhichTree.Right,
            "",
            "",
            smallTree,
            bigTree,
            smallToBig,
            5000
          )
          expect(res.selected).toEqual(8)
          expect(res.selectedTreeId).toEqual(WhichTree.Both)
        })

        it("29 Right -> 30 right", async () => {
          let res = await goLeftMerged(
            29,
            WhichTree.Right,
            "",
            "",
            smallTree,
            bigTree,
            smallToBig,
            5000
          )
          expect(res.selected).toEqual(30)
          expect(res.selectedTreeId).toEqual(WhichTree.Right)
        })
        it("31 Right -> 32 right", async () => {
          let res = await goLeftMerged(
            29,
            WhichTree.Right,
            "",
            "",
            smallTree,
            bigTree,
            smallToBig,
            5000
          )
          expect(res.selected).toEqual(30)
          expect(res.selectedTreeId).toEqual(WhichTree.Right)
        })
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
          fetchMock.resetMocks()
          fetchMock
            .once(JSON.stringify(coreOf3Normal))
            .once(JSON.stringify(coreOf3Sacbounds))
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

        it(" 6 Left -> 7 Left ", async () => {
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
          expect(res.selectedTreeId).toEqual(WhichTree.Left)
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

        describe("Right tree only", () => {
          it("4 Right -> 16", async () => {
            let res = await goLeftMerged(
              4,
              WhichTree.Right,
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
          it("7 Right -> 26", async () => {
            let res = await goLeftMerged(
              7,
              WhichTree.Right,
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

          it("10 Right -> 10 Right", async () => {
            let res = await goLeftMerged(
              10,
              WhichTree.Right,
              "",
              "",
              bigTree,
              smallTree,
              bigToSmall,
              5000
            )
            expect(res.selected).toEqual(10)
            expect(res.selectedTreeId).toEqual(WhichTree.Right)
          })
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
      it("4 Left -> 3", async () => {
        expect(
          goUpMerged(smallTree, bigTree, 4, WhichTree.Left, smallToBig)
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
          expect(res.selectedTreeId).toEqual(WhichTree.Right)
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
          expect(res.selected).toEqual(7)
          expect(res.selectedTreeId).toEqual(WhichTree.Right)
        })
        describe("Left: big | Right: small", () => {
          beforeEach(async () => {
            let res = await loadTreeBigOnLeftSmallOnRight()
            bigTree = res.bigTree
            smallTree = res.smallTree
          })
          it("3 -> 7 left ", async () => {
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
            expect(res.selected).toEqual(7)
            expect(res.selectedTreeId).toEqual(WhichTree.Left)
          })
          it("17 -> 21 left ", async () => {
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
            expect(res.selected).toEqual(21)
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

          it("3 -> 7 Right ", async () => {
            let res = await goRightMerged(
              smallTree,
              bigTree,
              3,
              WhichTree.Both,
              smallToBig
            )
            expect(res.selected).toEqual(7)
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
            expect(res.selected).toEqual(21)
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
            it("3 -> 4 Right ", async () => {
              let res = await goRightMerged(
                bigTree,
                smallTree,
                3,
                WhichTree.Both,
                bigToSmall
              )
              expect(res.selected).toEqual(4)
              expect(res.selectedTreeId).toEqual(WhichTree.Right)
            })
            it("17 -> 7 Right ", async () => {
              let res = await goRightMerged(
                bigTree,
                smallTree,
                17,
                WhichTree.Both,
                bigToSmall
              )
              expect(res.selected).toEqual(7)
              expect(res.selectedTreeId).toEqual(WhichTree.Right)
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
        })
      })
    })
  })

  describe("No diffs loaded", () => {
    describe("goleft", () => {
      describe("Left: small | Right: big", () => {
        beforeEach(async () => {
          fetchMock.resetMocks()
          fetchMock
            .once(JSON.stringify(coreOf3Sacbounds))
            .once(JSON.stringify(coreOf3Normal))
            .once(JSON.stringify(coreOf6Sacbounds))
            .once(JSON.stringify(coreOf17Normal))
            .once(JSON.stringify(coreOf9Sacbounds))
            .once(JSON.stringify(coreOf27Normal))

            .once(JSON.stringify(descendantsOf4Normal))
            .once(JSON.stringify(descendantsOf7Normal))
            .once(JSON.stringify(descendantsOf18Normal))
            .once(JSON.stringify(descendantsOf21Normal))
            .once(JSON.stringify(descendantsOf28Normal))
            .once(JSON.stringify([]))
        })
      })
    })
  })
})
