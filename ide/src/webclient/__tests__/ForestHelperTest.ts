import Node, { WhichTree } from "../src/modules/Node"
import { loadAllDiffs, mergeMaps, getDescList } from "../src/modules/ForestHelper"
import { fetchAncestors } from "../src/modules/MovementHelper"
import {
  coreOf3 as coreOf3Normal,
  coreOf17 as coreOf17Normal,
  coreOf27 as coreOf27Normal,
  descendantsOf28 as descendantsOf28Normal,
  core as coreNormal
} from "./resources/normal-8"
import {
  coreOf3 as coreOf3Sacbounds,
  coreOf6 as coreOf6Sacbounds,
  coreOf9 as coreOf9Sacbounds,
  core as coreSacbounds
} from "./resources/sacbounds-8"

import {
  core as coreFindAllSols
} from "./resources/findAllSols-8"

import { cloneDeep } from "lodash"
import { bigToSmall } from "./resources/normalVSSacbounds-8";
import { flipDiffLocations } from "../src/modules/Helper";
import { makeState } from "../src/modules/TreeHelper";

describe("testing ForestHelper", () => {
  const flipped = flipDiffLocations(bigToSmall)
  let bigTree: any
  let smallTree: any

  beforeEach(async () => {
    fetchMock.resetMocks()
    fetchMock
      .once(JSON.stringify(coreOf3Normal))
      .once(JSON.stringify(coreOf3Sacbounds))
      .once(JSON.stringify(coreOf17Normal))
      .once(JSON.stringify(coreOf6Sacbounds))
      .once(JSON.stringify(coreOf27Normal))
      .once(JSON.stringify(coreOf9Sacbounds))

    let res = await loadAllDiffs(
      ["", "s"],
      [coreNormal, coreSacbounds],
      bigToSmall,
      5000
    )

    bigTree = res[0]
    smallTree = res[1]
  })
  describe("testing mergeMaps", () => {
    it("It should not modify the left or right rees", async () => {
      let copyLeft = cloneDeep(bigTree)
      let copyRight = cloneDeep(smallTree)
      await mergeMaps(copyLeft, copyRight, bigToSmall, [])

      expect(copyLeft).toEqual(bigTree)
      expect(copyRight).toEqual(smallTree)
    })

    it("It should merge normal with findAllsols", async () => {
      let normalMap = makeState(coreNormal, 0).id2Node
      let findAllSolsMap = makeState(coreFindAllSols, 0).id2Node

      let merged = mergeMaps(normalMap, findAllSolsMap, [[32,32]], [33, 36])
      expect(merged[1].children!.map (x => x.id)).toEqual([2, 36])
      expect(merged[26].children!.map (x => x.id)).toEqual([27, 33])
      expect(merged[26].children![1].treeId).toEqual(WhichTree.Right)
      expect(merged[1].children![1].treeId).toEqual(WhichTree.Right)
    })

  it("It should merge findAllSols with normal", async () => {
      let normalMap = makeState(coreNormal, 0).id2Node
      let findAllSolsMap = makeState(coreFindAllSols, 0).id2Node

      let merged = mergeMaps( findAllSolsMap, normalMap, [[32, 32]], [] )
      expect(merged[1].children!.map (x => x.id)).toEqual([2, 36])
      expect(merged[26].children!.map (x => x.id)).toEqual([27, 33])
      expect(merged[26].children![1].treeId).toEqual(WhichTree.Left)
      expect(merged[1].children![1].treeId).toEqual(WhichTree.Left)
    })

    it("When the trees differ at the root there should not be any both for the treeid on any node", async () => {
      let res = await mergeMaps(cloneDeep(bigTree), cloneDeep(smallTree), [
        [0, 0]
      ], [])
      const nodeList = getDescList(res[0])
      const bothNodes = nodeList.find(x => x.data.treeId === WhichTree.Both)

      expect(bothNodes!.data.id).toEqual(-1)
    })

    it("Should merge the maps the ancestors of each tree into their maps big->small", async () => {
      let res = await mergeMaps(bigTree, smallTree, bigToSmall, [])

      expect(res[0].treeId).toBe(WhichTree.Both)
      expect(res[1].treeId).toBe(WhichTree.Both)
      expect(res[2].treeId).toBe(WhichTree.Both)
      expect(res[3].treeId).toBe(WhichTree.Both)
      expect(res[16].treeId).toBe(WhichTree.Both)
      expect(res[17].treeId).toBe(WhichTree.Both)
      expect(res[26].treeId).toBe(WhichTree.Both)
      expect(res[27].treeId).toBe(WhichTree.Both)





      let diff1 = getDescList(res[0]).find(
        x => x.data.id === bigToSmall[0][0]
      )!
      let diff2 = getDescList(res[0]).find(
        x => x.data.id === bigToSmall[1][0]
      )!
      let diff3 = getDescList(res[0]).find(
        x => x.data.id === bigToSmall[2][0]
      )!

      expect(diff1).toBeTruthy()

      expect(diff1.children!.map(x => x.data.id)).toEqual([4, 7, 4])
      expect(diff1.children!.map(x => x.data.treeId)).toEqual([
        WhichTree.Left,
        WhichTree.Left,
        WhichTree.Right
      ])
      expect(diff2.children!.map(x => x.data.id)).toEqual([18, 21, 7])
      expect(diff2.children!.map(x => x.data.treeId)).toEqual([
        WhichTree.Left,
        WhichTree.Left,
        WhichTree.Right
      ])
      expect(diff3.children!.map(x => x.data.id)).toEqual([28, 30, 10])
      expect(diff3.children!.map(x => x.data.treeId)).toEqual([
        WhichTree.Left,
        WhichTree.Left,
        WhichTree.Right
      ])
    })

    it("Should merge the maps the ancestors of each tree into their maps small->big", async () => {
      let res = await mergeMaps(
        smallTree,
        bigTree,
        flipped,
        []
      )
      let diff1 = getDescList(res[0]).find(
        x => x.data.id === bigToSmall[0][1]
      )!
      let diff2 = getDescList(res[0]).find(
        x => x.data.id === bigToSmall[1][1]
      )!
      let diff3 = getDescList(res[0]).find(
        x => x.data.id === bigToSmall[2][1]
      )!

      expect(diff1).toBeTruthy()

      expect(diff1.children!.map(x => x.data.id)).toEqual([4, 4, 7])
      expect(diff1.children!.map(x => x.data.treeId)).toEqual([
        WhichTree.Left,
        WhichTree.Right,
        WhichTree.Right
      ])
      expect(diff2.children!.map(x => x.data.id)).toEqual([7, 18, 21])
      expect(diff2.children!.map(x => x.data.treeId)).toEqual([
        WhichTree.Left,
        WhichTree.Right,
        WhichTree.Right
      ])
      expect(diff3.children!.map(x => x.data.id)).toEqual([10, 28, 30])
      expect(diff3.children!.map(x => x.data.treeId)).toEqual([
        WhichTree.Left,
        WhichTree.Right,
        WhichTree.Right
      ])
    })
  })

  describe("test fetch ancestors", () => {
    beforeEach(() => {
      fetchMock.resetMocks()
    })

    it("returns the ancestors (including the current node)", () => {
      fetchMock.mockResponseOnce(
        JSON.stringify([new Node(4, "poop", "", 3, 0, true, 0, false)])
      )

      fetchAncestors("", 3, 0).then(res =>
        expect(res).toEqual([
          {
            name: "",
            descCount: 0,
            treeId: 2,
            id: 4,
            x0: null,
            y0: null,
            parentId: 3,
            depth: 0,
            label: "poop",
            prettyLabel: "",
            isLeftChild: true,
            childCount: 0,
            isSolution: false
          }
        ])
      )
    })
  })

  describe("testing loadDiffs", () => {
    beforeEach(() => {
      fetchMock.resetMocks()
    })

    it("Should load the ancestors of each tree into their maps", async () => {
      fetchMock.once(JSON.stringify(coreOf3Normal))
      .once(JSON.stringify(coreOf3Sacbounds))
      .once(JSON.stringify(coreOf17Normal))
      .once(JSON.stringify(coreOf6Sacbounds))
      .once(JSON.stringify(coreOf27Normal))
      .once(JSON.stringify(coreOf9Sacbounds))

      let res = await loadAllDiffs(
        ["", "s"],
        [coreNormal, coreSacbounds],
        bigToSmall,
        5000
      )

      let leftTree = res[0]
      let rightTree = res[1]
      

      // console.log(rightTree)
      expect(leftTree[21].treeId).toEqual(WhichTree.Left)

      expect(4 in leftTree)
      expect(7 in leftTree)
      expect(18 in leftTree)
      expect(21 in leftTree)
      expect(28 in leftTree)
      expect(30 in leftTree)

      expect(leftTree[3].children!.map(x => x.id)).toEqual([4, 7])
      expect(leftTree[17].children!.map(x => x.id)).toEqual([18, 21])
      expect(leftTree[27].children!.map(x => x.id)).toEqual([28, 30])

      expect(10 in leftTree)
      expect(4 in leftTree)
      expect(7 in leftTree)

      expect(rightTree[9].children!.map(x => x.id)).toEqual([10])
      expect(rightTree[3].children!.map(x => x.id)).toEqual([4])
      expect(rightTree[6].children!.map(x => x.id)).toEqual([7])

      // console.log(JSON.stringify(rightTree))
    })
  })
})