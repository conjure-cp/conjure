import Node, { WhichTree } from "../src/modules/Node"
import { loadDiffs, mergeMaps, getDescList } from "../src/modules/ForestHelper"
import { fetchAncestors } from "../src/modules/MovementHelper"
// import { FetchMock} from "jest-fetch-mock"
import { cloneDeep } from "lodash"
import * as d3 from "d3"
import { node } from "prop-types"

describe("testing mergeMaps", () => {
  let bigTree: any
  let smallTree: any

  beforeEach(async () => {
    fetchMock.resetMocks()
    fetchMock
      .once(JSON.stringify(leftAncestors3))
      .once(JSON.stringify(leftAncestors17))
      .once(JSON.stringify(leftAncestors27))
      .once(JSON.stringify(rightAncestors9))
      .once(JSON.stringify(rightAncestors3))
      .once(JSON.stringify(rightAncestors6))

    let res = await loadDiffs(
      ["", "s"],
      [normal, sacBounds],
      diffLocations,
      5000
    )

    bigTree = res[0]
    smallTree = res[1]
  })

  it("It should not modify the left or right rees", async () => {
    let copyLeft = cloneDeep(bigTree)
    let copyRight = cloneDeep(smallTree)
    await mergeMaps(copyLeft, copyRight, diffLocations)

    expect(copyLeft).toEqual(bigTree)
    expect(copyRight).toEqual(smallTree)
  })

  it("When the trees differ at the root there should not be any both for the treeid on any node", async () => {
    let res = await mergeMaps(cloneDeep(bigTree), cloneDeep(smallTree), [
      [0, 0]
    ])
    const nodeList = getDescList(res[0])
    const bothNodes = nodeList.find(x => x.data.treeId === WhichTree.Both)

    expect(bothNodes!.data.id).toEqual(-1)
  })

  it("Should merge the maps the ancestors of each tree into their maps big->small", async () => {
    let res = await mergeMaps(bigTree, smallTree, diffLocations)
    let diff1 = getDescList(res[0]).find(
      x => x.data.id === diffLocations[0][0]
    )!
    let diff2 = getDescList(res[0]).find(
      x => x.data.id === diffLocations[1][0]
    )!
    let diff3 = getDescList(res[0]).find(
      x => x.data.id === diffLocations[2][0]
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
    let res = await mergeMaps(smallTree, bigTree, diffLocations.map(x => [x[1], x[0]]))
    let diff1 = getDescList(res[0]).find(
      x => x.data.id === diffLocations[0][1]
    )!
    let diff2 = getDescList(res[0]).find(
      x => x.data.id === diffLocations[1][1]
    )!
    let diff3 = getDescList(res[0]).find(
      x => x.data.id === diffLocations[2][1]
    )!

    expect(diff1).toBeTruthy()

    expect(diff1.children!.map(x => x.data.id)).toEqual([4, 4, 7])
    expect(diff1.children!.map(x => x.data.treeId)).toEqual([
      WhichTree.Left,
      WhichTree.Right,
      WhichTree.Right,
    ])
    expect(diff2.children!.map(x => x.data.id)).toEqual([7, 18, 21])
    expect(diff2.children!.map(x => x.data.treeId)).toEqual([
      WhichTree.Left,
      WhichTree.Right,
      WhichTree.Right,
    ])
    expect(diff3.children!.map(x => x.data.id)).toEqual([10, 28, 30])
    expect(diff3.children!.map(x => x.data.treeId)).toEqual([
      WhichTree.Left,
      WhichTree.Right,
      WhichTree.Right,
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
    fetchMock
      .once(JSON.stringify(leftAncestors3))
      .once(JSON.stringify(leftAncestors17))
      .once(JSON.stringify(leftAncestors27))
      .once(JSON.stringify(rightAncestors9))
      .once(JSON.stringify(rightAncestors3))
      .once(JSON.stringify(rightAncestors6))

    let res = await loadDiffs(
      ["", "s"],
      [normal, sacBounds],
      diffLocations,
      5000
    )

    let leftTree = res[0]
    let rightTree = res[1]

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

const diffLocations = [[3, 3], [17, 6], [27, 9]]

const rightAncestors9 = [
  {
    id: 0,
    parentId: -1,
    label: "",
    prettyLabel: "",
    childCount: 1,
    isSolution: false,
    isLeftChild: true,
    descCount: 10
  },
  {
    id: 1,
    parentId: 0,
    label: "Root Propagation",
    prettyLabel: "Root Propagation",
    childCount: 1,
    isSolution: false,
    isLeftChild: true,
    descCount: 9
  },
  {
    id: 2,
    parentId: 1,
    label: "setA_Occurrence_00001 = 0",
    prettyLabel: "setA_Occurrence_00001 = 0",
    childCount: 2,
    isSolution: false,
    isLeftChild: true,
    descCount: 8
  },
  {
    id: 3,
    parentId: 2,
    label: "setA_Occurrence_00002 = 0",
    prettyLabel: "setA_Occurrence_00002 = 0",
    childCount: 1,
    isSolution: false,
    isLeftChild: true,
    descCount: 1
  },
  {
    id: 5,
    parentId: 2,
    label: "setA_Occurrence_00002 != 0",
    prettyLabel: "setA_Occurrence_00002 != 0",
    childCount: 2,
    isSolution: false,
    isLeftChild: false,
    descCount: 5
  },
  {
    id: 6,
    parentId: 5,
    label: "setA_Occurrence_00003 = 0",
    prettyLabel: "setA_Occurrence_00003 = 0",
    childCount: 1,
    isSolution: false,
    isLeftChild: true,
    descCount: 1
  },
  {
    id: 8,
    parentId: 5,
    label: "setA_Occurrence_00003 != 0",
    prettyLabel: "setA_Occurrence_00003 != 0",
    childCount: 1,
    isSolution: false,
    isLeftChild: false,
    descCount: 2
  },
  {
    id: 9,
    parentId: 8,
    label: "setA_Occurrence_00004 = 0",
    prettyLabel: "setA_Occurrence_00004 = 0",
    childCount: 1,
    isSolution: false,
    isLeftChild: true,
    descCount: 1
  },
  {
    id: 10,
    parentId: 9,
    label: "setA_Occurrence_00005 != 0",
    prettyLabel: "setA_Occurrence_00005 != 0",
    childCount: 0,
    isSolution: true,
    isLeftChild: false,
    descCount: 0
  }
]

const leftAncestors27 = [
  {
    id: 0,
    parentId: -1,
    label: "",
    prettyLabel: "",
    childCount: 1,
    isSolution: false,
    isLeftChild: true,
    descCount: 32
  },
  {
    id: 1,
    parentId: 0,
    label: "Root Propagation",
    prettyLabel: "Root Propagation",
    childCount: 1,
    isSolution: false,
    isLeftChild: true,
    descCount: 31
  },
  {
    id: 2,
    parentId: 1,
    label: "setA_Occurrence_00001 = 0",
    prettyLabel: "setA_Occurrence_00001 = 0",
    childCount: 2,
    isSolution: false,
    isLeftChild: true,
    descCount: 30
  },
  {
    id: 3,
    parentId: 2,
    label: "setA_Occurrence_00002 = 0",
    prettyLabel: "setA_Occurrence_00002 = 0",
    childCount: 2,
    isSolution: false,
    isLeftChild: true,
    descCount: 12
  },
  {
    id: 16,
    parentId: 2,
    label: "setA_Occurrence_00002 != 0",
    prettyLabel: "setA_Occurrence_00002 != 0",
    childCount: 2,
    isSolution: false,
    isLeftChild: false,
    descCount: 16
  },
  {
    id: 17,
    parentId: 16,
    label: "setA_Occurrence_00003 = 0",
    prettyLabel: "setA_Occurrence_00003 = 0",
    childCount: 2,
    isSolution: false,
    isLeftChild: true,
    descCount: 8
  },
  {
    id: 26,
    parentId: 16,
    label: "setA_Occurrence_00003 != 0",
    prettyLabel: "setA_Occurrence_00003 != 0",
    childCount: 1,
    isSolution: false,
    isLeftChild: false,
    descCount: 6
  },
  {
    id: 27,
    parentId: 26,
    label: "setA_Occurrence_00004 = 0",
    prettyLabel: "setA_Occurrence_00004 = 0",
    childCount: 2,
    isSolution: false,
    isLeftChild: true,
    descCount: 5
  },
  {
    id: 28,
    parentId: 27,
    label: "setA_Occurrence_00005 = 0",
    prettyLabel: "setA_Occurrence_00005 = 0",
    childCount: 1,
    isSolution: false,
    isLeftChild: true,
    descCount: 1
  },
  {
    id: 30,
    parentId: 27,
    label: "setA_Occurrence_00005 != 0",
    prettyLabel: "setA_Occurrence_00005 != 0",
    childCount: 1,
    isSolution: false,
    isLeftChild: false,
    descCount: 2
  }
]

const rightAncestors6 = [
  {
    id: 0,
    parentId: -1,
    label: "",
    prettyLabel: "",
    childCount: 1,
    isSolution: false,
    isLeftChild: true,
    descCount: 10
  },
  {
    id: 1,
    parentId: 0,
    label: "Root Propagation",
    prettyLabel: "Root Propagation",
    childCount: 1,
    isSolution: false,
    isLeftChild: true,
    descCount: 9
  },
  {
    id: 2,
    parentId: 1,
    label: "setA_Occurrence_00001 = 0",
    prettyLabel: "setA_Occurrence_00001 = 0",
    childCount: 2,
    isSolution: false,
    isLeftChild: true,
    descCount: 8
  },
  {
    id: 3,
    parentId: 2,
    label: "setA_Occurrence_00002 = 0",
    prettyLabel: "setA_Occurrence_00002 = 0",
    childCount: 1,
    isSolution: false,
    isLeftChild: true,
    descCount: 1
  },
  {
    id: 5,
    parentId: 2,
    label: "setA_Occurrence_00002 != 0",
    prettyLabel: "setA_Occurrence_00002 != 0",
    childCount: 2,
    isSolution: false,
    isLeftChild: false,
    descCount: 5
  },
  {
    id: 6,
    parentId: 5,
    label: "setA_Occurrence_00003 = 0",
    prettyLabel: "setA_Occurrence_00003 = 0",
    childCount: 1,
    isSolution: false,
    isLeftChild: true,
    descCount: 1
  },
  {
    id: 7,
    parentId: 6,
    label: "setA_Occurrence_00004 != 0",
    prettyLabel: "setA_Occurrence_00004 != 0",
    childCount: 0,
    isSolution: false,
    isLeftChild: false,
    descCount: 0
  },
  {
    id: 8,
    parentId: 5,
    label: "setA_Occurrence_00003 != 0",
    prettyLabel: "setA_Occurrence_00003 != 0",
    childCount: 1,
    isSolution: false,
    isLeftChild: false,
    descCount: 2
  }
]

const leftAncestors17 = [
  {
    id: 0,
    parentId: -1,
    label: "",
    prettyLabel: "",
    childCount: 1,
    isSolution: false,
    isLeftChild: true,
    descCount: 32
  },
  {
    id: 1,
    parentId: 0,
    label: "Root Propagation",
    prettyLabel: "Root Propagation",
    childCount: 1,
    isSolution: false,
    isLeftChild: true,
    descCount: 31
  },
  {
    id: 2,
    parentId: 1,
    label: "setA_Occurrence_00001 = 0",
    prettyLabel: "setA_Occurrence_00001 = 0",
    childCount: 2,
    isSolution: false,
    isLeftChild: true,
    descCount: 30
  },
  {
    id: 3,
    parentId: 2,
    label: "setA_Occurrence_00002 = 0",
    prettyLabel: "setA_Occurrence_00002 = 0",
    childCount: 2,
    isSolution: false,
    isLeftChild: true,
    descCount: 12
  },
  {
    id: 16,
    parentId: 2,
    label: "setA_Occurrence_00002 != 0",
    prettyLabel: "setA_Occurrence_00002 != 0",
    childCount: 2,
    isSolution: false,
    isLeftChild: false,
    descCount: 16
  },
  {
    id: 17,
    parentId: 16,
    label: "setA_Occurrence_00003 = 0",
    prettyLabel: "setA_Occurrence_00003 = 0",
    childCount: 2,
    isSolution: false,
    isLeftChild: true,
    descCount: 8
  },
  {
    id: 18,
    parentId: 17,
    label: "setA_Occurrence_00004 = 0",
    prettyLabel: "setA_Occurrence_00004 = 0",
    childCount: 1,
    isSolution: false,
    isLeftChild: true,
    descCount: 2
  },
  {
    id: 21,
    parentId: 17,
    label: "setA_Occurrence_00004 != 0",
    prettyLabel: "setA_Occurrence_00004 != 0",
    childCount: 2,
    isSolution: false,
    isLeftChild: false,
    descCount: 4
  },
  {
    id: 26,
    parentId: 16,
    label: "setA_Occurrence_00003 != 0",
    prettyLabel: "setA_Occurrence_00003 != 0",
    childCount: 1,
    isSolution: false,
    isLeftChild: false,
    descCount: 6
  }
]

const rightAncestors3 = [
  {
    id: 0,
    parentId: -1,
    label: "",
    prettyLabel: "",
    childCount: 1,
    isSolution: false,
    isLeftChild: true,
    descCount: 10
  },
  {
    id: 1,
    parentId: 0,
    label: "Root Propagation",
    prettyLabel: "Root Propagation",
    childCount: 1,
    isSolution: false,
    isLeftChild: true,
    descCount: 9
  },
  {
    id: 2,
    parentId: 1,
    label: "setA_Occurrence_00001 = 0",
    prettyLabel: "setA_Occurrence_00001 = 0",
    childCount: 2,
    isSolution: false,
    isLeftChild: true,
    descCount: 8
  },
  {
    id: 3,
    parentId: 2,
    label: "setA_Occurrence_00002 = 0",
    prettyLabel: "setA_Occurrence_00002 = 0",
    childCount: 1,
    isSolution: false,
    isLeftChild: true,
    descCount: 1
  },
  {
    id: 4,
    parentId: 3,
    label: "setA_Occurrence_00003 != 0",
    prettyLabel: "setA_Occurrence_00003 != 0",
    childCount: 0,
    isSolution: false,
    isLeftChild: false,
    descCount: 0
  },
  {
    id: 5,
    parentId: 2,
    label: "setA_Occurrence_00002 != 0",
    prettyLabel: "setA_Occurrence_00002 != 0",
    childCount: 2,
    isSolution: false,
    isLeftChild: false,
    descCount: 5
  }
]

const leftAncestors3 = [
  {
    id: 0,
    parentId: -1,
    label: "",
    prettyLabel: "",
    childCount: 1,
    isSolution: false,
    isLeftChild: true,
    descCount: 32
  },
  {
    id: 1,
    parentId: 0,
    label: "Root Propagation",
    prettyLabel: "Root Propagation",
    childCount: 1,
    isSolution: false,
    isLeftChild: true,
    descCount: 31
  },
  {
    id: 2,
    parentId: 1,
    label: "setA_Occurrence_00001 = 0",
    prettyLabel: "setA_Occurrence_00001 = 0",
    childCount: 2,
    isSolution: false,
    isLeftChild: true,
    descCount: 30
  },
  {
    id: 3,
    parentId: 2,
    label: "setA_Occurrence_00002 = 0",
    prettyLabel: "setA_Occurrence_00002 = 0",
    childCount: 2,
    isSolution: false,
    isLeftChild: true,
    descCount: 12
  },
  {
    id: 4,
    parentId: 3,
    label: "setA_Occurrence_00003 = 0",
    prettyLabel: "setA_Occurrence_00003 = 0",
    childCount: 1,
    isSolution: false,
    isLeftChild: true,
    descCount: 2
  },
  {
    id: 7,
    parentId: 3,
    label: "setA_Occurrence_00003 != 0",
    prettyLabel: "setA_Occurrence_00003 != 0",
    childCount: 2,
    isSolution: false,
    isLeftChild: false,
    descCount: 8
  },
  {
    id: 16,
    parentId: 2,
    label: "setA_Occurrence_00002 != 0",
    prettyLabel: "setA_Occurrence_00002 != 0",
    childCount: 2,
    isSolution: false,
    isLeftChild: false,
    descCount: 16
  }
]

let normal = {
  nodes: [
    {
      id: 0,
      parentId: -1,
      label: "",
      prettyLabel: "",
      childCount: 1,
      isSolution: false,
      isLeftChild: true,
      descCount: 32
    },
    {
      id: 1,
      parentId: 0,
      label: "Root Propagation",
      prettyLabel: "Root Propagation",
      childCount: 1,
      isSolution: false,
      isLeftChild: true,
      descCount: 31
    },
    {
      id: 2,
      parentId: 1,
      label: "setA_Occurrence_00001 = 0",
      prettyLabel: "setA_Occurrence_00001 = 0",
      childCount: 2,
      isSolution: false,
      isLeftChild: true,
      descCount: 30
    },
    {
      id: 16,
      parentId: 2,
      label: "setA_Occurrence_00002 != 0",
      prettyLabel: "setA_Occurrence_00002 != 0",
      childCount: 2,
      isSolution: false,
      isLeftChild: false,
      descCount: 16
    },
    {
      id: 26,
      parentId: 16,
      label: "setA_Occurrence_00003 != 0",
      prettyLabel: "setA_Occurrence_00003 != 0",
      childCount: 1,
      isSolution: false,
      isLeftChild: false,
      descCount: 6
    },
    {
      id: 27,
      parentId: 26,
      label: "setA_Occurrence_00004 = 0",
      prettyLabel: "setA_Occurrence_00004 = 0",
      childCount: 2,
      isSolution: false,
      isLeftChild: true,
      descCount: 5
    },
    {
      id: 30,
      parentId: 27,
      label: "setA_Occurrence_00005 != 0",
      prettyLabel: "setA_Occurrence_00005 != 0",
      childCount: 1,
      isSolution: false,
      isLeftChild: false,
      descCount: 2
    },
    {
      id: 31,
      parentId: 30,
      label: "setA_Occurrence_00006 = 0",
      prettyLabel: "setA_Occurrence_00006 = 0",
      childCount: 1,
      isSolution: false,
      isLeftChild: true,
      descCount: 1
    },
    {
      id: 32,
      parentId: 31,
      label: "setA_Occurrence_00007 = 0",
      prettyLabel: "setA_Occurrence_00007 = 0",
      childCount: 0,
      isSolution: true,
      isLeftChild: true,
      descCount: 0
    },
    {
      id: 3,
      parentId: 2,
      label: "setA_Occurrence_00002 = 0",
      prettyLabel: "setA_Occurrence_00002 = 0",
      childCount: 2,
      isSolution: false,
      isLeftChild: true,
      descCount: 12
    },
    {
      id: 17,
      parentId: 16,
      label: "setA_Occurrence_00003 = 0",
      prettyLabel: "setA_Occurrence_00003 = 0",
      childCount: 2,
      isSolution: false,
      isLeftChild: true,
      descCount: 8
    },
    {
      id: 28,
      parentId: 27,
      label: "setA_Occurrence_00005 = 0",
      prettyLabel: "setA_Occurrence_00005 = 0",
      childCount: 1,
      isSolution: false,
      isLeftChild: true,
      descCount: 1
    }
  ],
  solAncestorIds: [0, 1, 2, 16, 26, 27, 30, 31, 32]
}

let sacBounds = {
  nodes: [
    {
      id: 0,
      parentId: -1,
      label: "",
      prettyLabel: "",
      childCount: 1,
      isSolution: false,
      isLeftChild: true,
      descCount: 10
    },
    {
      id: 1,
      parentId: 0,
      label: "Root Propagation",
      prettyLabel: "Root Propagation",
      childCount: 1,
      isSolution: false,
      isLeftChild: true,
      descCount: 9
    },
    {
      id: 2,
      parentId: 1,
      label: "setA_Occurrence_00001 = 0",
      prettyLabel: "setA_Occurrence_00001 = 0",
      childCount: 2,
      isSolution: false,
      isLeftChild: true,
      descCount: 8
    },
    {
      id: 5,
      parentId: 2,
      label: "setA_Occurrence_00002 != 0",
      prettyLabel: "setA_Occurrence_00002 != 0",
      childCount: 2,
      isSolution: false,
      isLeftChild: false,
      descCount: 5
    },
    {
      id: 8,
      parentId: 5,
      label: "setA_Occurrence_00003 != 0",
      prettyLabel: "setA_Occurrence_00003 != 0",
      childCount: 1,
      isSolution: false,
      isLeftChild: false,
      descCount: 2
    },
    {
      id: 9,
      parentId: 8,
      label: "setA_Occurrence_00004 = 0",
      prettyLabel: "setA_Occurrence_00004 = 0",
      childCount: 1,
      isSolution: false,
      isLeftChild: true,
      descCount: 1
    },
    {
      id: 10,
      parentId: 9,
      label: "setA_Occurrence_00005 != 0",
      prettyLabel: "setA_Occurrence_00005 != 0",
      childCount: 0,
      isSolution: true,
      isLeftChild: false,
      descCount: 0
    },
    {
      id: 3,
      parentId: 2,
      label: "setA_Occurrence_00002 = 0",
      prettyLabel: "setA_Occurrence_00002 = 0",
      childCount: 1,
      isSolution: false,
      isLeftChild: true,
      descCount: 1
    },
    {
      id: 6,
      parentId: 5,
      label: "setA_Occurrence_00003 = 0",
      prettyLabel: "setA_Occurrence_00003 = 0",
      childCount: 1,
      isSolution: false,
      isLeftChild: true,
      descCount: 1
    }
  ],
  solAncestorIds: [0, 1, 2, 5, 8, 9, 10]
}
