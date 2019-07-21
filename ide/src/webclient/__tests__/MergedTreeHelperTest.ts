import Node, { WhichTree } from "../src/modules/Node"
import { loadDiffs, mergeMaps, getDescList } from "../src/modules/ForestHelper"
import { fetchAncestors } from "../src/modules/MovementHelper"
// import { FetchMock} from "jest-fetch-mock"
import { cloneDeep } from "lodash"
import * as d3 from "d3"
import { node } from "prop-types"
import {
  goLeftAtDiffingPoint,
  reviseGoLeft
} from "../src/modules/MergedTreeHelper"
import { flipDiffLocations } from "../src/modules/Helper"

const diffLocations = [[3, 3], [17, 6], [27, 9]]

describe("test revise goleft, it redirects to a node to the right of the next diff location", () => {
  let bigTree: any
  let smallTree: any
  let flipped = flipDiffLocations(diffLocations)

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

  it("returns the correct ids small -> big", async () => {
    const merged = mergeMaps(smallTree, bigTree, flipped)
    let res = reviseGoLeft(merged, 4, 5, WhichTree.Left, flipped)
    expect(res).toEqual({ selected: 5, treeId: WhichTree.Both })

    res = reviseGoLeft(merged, 7, 8, WhichTree.Left, flipped)
    expect(res).toEqual({ selected: 8, treeId: WhichTree.Both })

    res = reviseGoLeft(merged, 10, 10, WhichTree.Left, flipped)
    expect(res).toEqual({ selected: 10, treeId: WhichTree.Left })
  })

  it("returns the correct ids big -> small", async () => {
    const merged = mergeMaps(bigTree, smallTree, diffLocations)
    let res = reviseGoLeft(merged, 4, 5, WhichTree.Right, diffLocations)
    expect(res).toEqual({ selected: 16, treeId: WhichTree.Both })

    res = reviseGoLeft(merged, 7, 8, WhichTree.Right, diffLocations)
    expect(res).toEqual({ selected: 26, treeId: WhichTree.Both })

    res = reviseGoLeft(merged, 10, 10, WhichTree.Right, diffLocations)
    expect(res).toEqual({ selected: 10, treeId: WhichTree.Right })
  })
})

describe("test go left at diffing point", () => {
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
    let mergeMap = await mergeMaps(
      smallTree,
      bigTree,
      diffLocations.map(x => [x[1], x[0]])
    )
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
