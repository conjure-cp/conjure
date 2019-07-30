import Node, { WhichTree } from "../src/modules/Node"
import {
  loadAllDiffs,
  mergeMaps,
  getDescList,
  assignTreeIds
} from "../src/modules/ForestHelper"
import { fetchAncestors } from "../src/modules/MovementHelper"
import {
  coreOf3 as coreOf3Normal,
  coreOf7 as coreOf7Normal,
  coreOf17 as coreOf17Normal,
  coreOf21 as coreOf21Normal,
  coreOf27 as coreOf27Normal,
  descendantsOf28 as descendantsOf28Normal,
  core as coreNormal8
} from "./resources/normal-8"
import {
  coreOf3 as coreOf3Sacbounds,
  coreOf4 as coreOf4Sacbounds,
  coreOf6 as coreOf6Sacbounds,
  coreOf7 as coreOf7Sacbounds,
  coreOf9 as coreOf9Sacbounds,
  core as coreSacbounds8
} from "./resources/sacbounds-8"

import { core as coreFindAllSols } from "./resources/findAllSols-8"
import { core as coreNormal10 } from "./resources/normal-10"
import { core as coreSacbounds10 } from "./resources/sacbounds-10"

import { diffPoints as completelyDifferent } from "./resources/completelyDifferent"

import { cloneDeep } from "lodash"
import { bigToSmall as normalToSacbounds } from "./resources/normalVSSacbounds-8"
import { diffPoints as normalToFindAllSols } from "./resources/normalVSFindAllSols-8"
import { flipDiffLocations } from "../src/modules/Helper"
import { makeState } from "../src/modules/TreeHelper"

describe("testing ForestHelper", () => {
  const smallToBig = flipDiffLocations(normalToSacbounds)
  let bigTree: any
  let smallTree: any

  beforeEach(async () => {
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

    let res = await loadAllDiffs(
      ["", "s"],
      [coreNormal8, coreSacbounds8],
      normalToSacbounds,
      5000
    )

    bigTree = res[0]
    smallTree = res[1]
  })

  describe("testing mergeMaps", () => {
    it("It should not modify the left or right rees", async () => {
      let copyLeft = cloneDeep(bigTree)
      let copyRight = cloneDeep(smallTree)
      mergeMaps(copyLeft, copyRight, normalToSacbounds)

      expect(copyLeft).toEqual(bigTree)
      expect(copyRight).toEqual(smallTree)
    })

    it("It should merge normal with findAllsols", async () => {
      let normalMap = makeState(coreNormal8, 0).id2Node
      let findAllSolsMap = makeState(coreFindAllSols, 0).id2Node
      assignTreeIds(normalMap, findAllSolsMap, normalToFindAllSols)
      let merged = mergeMaps(normalMap, findAllSolsMap, normalToFindAllSols)

      expect(merged[1].children!.map(x => x.id)).toEqual([2, 36])
      expect(merged[26].children!.map(x => x.id)).toEqual([27, 33])
      expect(merged[26].children![1].treeId).toEqual(WhichTree.Right)
      expect(merged[1].children![1].treeId).toEqual(WhichTree.Right)
    })

    it("It should merge findAllSols with normal", async () => {
      let normalMap = makeState(coreNormal8, 0).id2Node
      let findAllSolsMap = makeState(coreFindAllSols, 0).id2Node

      const flipped = flipDiffLocations(normalToFindAllSols)

      assignTreeIds(findAllSolsMap, normalMap, flipped)

      let merged = mergeMaps(findAllSolsMap, normalMap, flipped)
      expect(merged[1].children!.map(x => x.id)).toEqual([2, 36])
      expect(merged[26].children!.map(x => x.id)).toEqual([27, 33])
      expect(merged[26].children![1].treeId).toEqual(WhichTree.Left)
      expect(merged[1].children![1].treeId).toEqual(WhichTree.Left)
    })

    it("When the trees differ at the root there should not be any both for the treeid on any node", async () => {
      let lMap = cloneDeep(bigTree)
      let rMap = cloneDeep(smallTree)

      assignTreeIds(lMap, rMap, completelyDifferent)

      let res = await mergeMaps(lMap, rMap, completelyDifferent)
      const nodeList = getDescList(res[0])
      const bothNodes = nodeList.find(x => x.treeId === WhichTree.Both)

      expect(res[0].treeId).toEqual(WhichTree.Both)
      // expect(bothNodes).toBeUndefined()
    })

    it("Should merge the maps the ancestors of each tree into their maps big->small", async () => {
      let lMap = cloneDeep(bigTree)
      let rMap = cloneDeep(smallTree)
      assignTreeIds(lMap, rMap, normalToSacbounds)
      let res = mergeMaps(bigTree, smallTree, normalToSacbounds)

      expect(res[0].treeId).toBe(WhichTree.Both)
      expect(res[1].treeId).toBe(WhichTree.Both)
      expect(res[2].treeId).toBe(WhichTree.Both)
      expect(res[3].treeId).toBe(WhichTree.Both)
      expect(res[16].treeId).toBe(WhichTree.Both)
      expect(res[17].treeId).toBe(WhichTree.Both)
      expect(res[26].treeId).toBe(WhichTree.Both)
      expect(res[27].treeId).toBe(WhichTree.Both)

      let diff1 = getDescList(res[0]).find(
        x => x.id === normalToSacbounds[0].leftTreeId
      )!
      let diff2 = getDescList(res[0]).find(
        x => x.id === normalToSacbounds[1].leftTreeId
      )!
      let diff3 = getDescList(res[0]).find(
        x => x.id === normalToSacbounds[2].leftTreeId
      )!
      let diff4 = getDescList(res[0]).find(
        x => x.id === normalToSacbounds[3].leftTreeId
      )!
      let diff5 = getDescList(res[0]).find(
        x => x.id === normalToSacbounds[4].leftTreeId
      )!

      expect(diff1).toBeTruthy()

      expect(diff1.children!.map(x => x.id)).toEqual([4, 7])
      expect(diff1.children!.map(x => x.treeId)).toEqual([
        WhichTree.Left,
        WhichTree.Both
      ])
      expect(diff2.children!.map(x => x.id)).toEqual([8, 11])
      expect(diff2.children!.map(x => x.treeId)).toEqual([
        WhichTree.Left,
        WhichTree.Left,
      ])
      expect(diff3.children!.map(x => x.id)).toEqual([18, 21])
      expect(diff3.children!.map(x => x.treeId)).toEqual([
        WhichTree.Left,
        WhichTree.Both
      ])

      expect(diff4.children!.map(x => x.id)).toEqual([22, 24])
      expect(diff4.children!.map(x => x.treeId)).toEqual([
        WhichTree.Left,
        WhichTree.Left
      ])

      expect(diff5.children!.map(x => x.id)).toEqual([28, 30, 10])

      expect(diff5.children!.map(x => x.treeId)).toEqual([
        WhichTree.Left,
        WhichTree.Left,
        WhichTree.Right,
      ])
    })

    it("Should merge the maps the ancestors of each tree into their maps small->big", async () => {
      let lMap = cloneDeep(smallTree)
      let rMap = cloneDeep(bigTree)
      assignTreeIds(lMap, rMap, smallToBig)
      let res = mergeMaps(lMap, rMap, smallToBig)
      let diff1 = getDescList(res[0]).find(
        x => x.id === normalToSacbounds[0].rightTreeId
      )!
      let diff2 = getDescList(res[0]).find(
        x => x.id === normalToSacbounds[1].rightTreeId
      )!
      let diff3 = getDescList(res[0]).find(
        x => x.id === normalToSacbounds[2].rightTreeId
      )!
      let diff4 = getDescList(res[0]).find(
        x => x.id === normalToSacbounds[3].rightTreeId
      )!
      let diff5 = getDescList(res[0]).find(
        x => x.id === normalToSacbounds[4].rightTreeId
      )!

      expect(diff1).toBeTruthy()

      expect(diff1.children!.map(x => x.id)).toEqual([4, 4])
      expect(diff1.children!.map(x => x.treeId)).toEqual([
        WhichTree.Both,
        WhichTree.Right
      ])
      expect(diff2.children!.map(x => x.id)).toEqual([8, 11])
      expect(diff2.children!.map(x => x.treeId)).toEqual([
        WhichTree.Right,
        WhichTree.Right
      ])
      expect(diff3.children!.map(x => x.id)).toEqual([7, 18])
      expect(diff3.children!.map(x => x.treeId)).toEqual([
        WhichTree.Both,
        WhichTree.Right
      ])

      expect(diff4.children!.map(x => x.id)).toEqual([22, 24])
      expect(diff4.children!.map(x => x.treeId)).toEqual([
        WhichTree.Right,
        WhichTree.Right
      ])
      expect(diff5.children!.map(x => x.id)).toEqual([10, 28, 30])
      expect(diff5.children!.map(x => x.treeId)).toEqual([
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

      let res = await loadAllDiffs(
        ["", "s"],
        [coreNormal8, coreSacbounds8],
        normalToSacbounds,
        5000
      )

      let leftTree = res[0]
      let rightTree = res[1]

      // console.log(rightTree)
      expect(leftTree[21].treeId).toEqual(WhichTree.Both)

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
