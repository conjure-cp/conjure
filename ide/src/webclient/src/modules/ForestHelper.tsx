import { makeState, insertNodesBoyo } from "./TreeHelper"
import Node, { WhichTree } from "./Node"
import { Core, MyMap } from "../components/vis/TreeContainer"
import { fetchAncestors } from "./MovementHelper"
import { isEqual, cloneDeep } from "lodash"
import * as d3 from "d3"
import { array } from "yup"

export const assignTreeIds = (
  leftMap: MyMap,
  rightMap: MyMap,
  diffLocations: number[][],
  augmentedIds: number[][]
) => {
  if (isEqual(diffLocations, [[-1, -1]])) {
    leftMap[0].treeId = WhichTree.Left
    rightMap[0].treeId = WhichTree.Right
    getDescList(leftMap[0]).forEach(x => (x.data.treeId = WhichTree.Left))
    getDescList(rightMap[0]).forEach(x => (x.data.treeId = WhichTree.Right))

    return
  }

  diffLocations.forEach(array => {
    if (leftMap[array[0]]) {
      getDescList(leftMap[array[0]]).forEach(
        x => (x.data.treeId = WhichTree.Left)
      )
    }
    if (rightMap[array[1]]) {
      getDescList(rightMap[array[1]]).forEach(
        x => (x.data.treeId = WhichTree.Right)
      )
    }
  })

  console.log(augmentedIds)

  augmentedIds[0].forEach(augId => {
    if (!(augId in leftMap)) {
      return
    }
    getDescList(leftMap[augId]).forEach(x => {
      x.data.treeId = WhichTree.Left
    })
  })

  augmentedIds[1].forEach(augId => {
    if (!(augId in rightMap)) {
      return
    }
    getDescList(rightMap[augId]).forEach(x => {
      x.data.treeId = WhichTree.Right
    })
  })

  // if (lIsBigger) {
  //   if (diffLocations[0][0] === rightMap[0].descCount) {
  //     // if (lIsBigger) {
  //     getDescList(leftMap[0])
  //       .filter(x => x.data.id > rightMap[0].descCount)
  //       .forEach(x => {
  //         x.data.treeId = WhichTree.Left
  //       })
  //   }
  // } else {
  //   augmentedIds.forEach(id => {
  //     let node = rightMap[id]
  //     node.treeId = WhichTree.Right
  //   })
  // }
}

export const getDescList = (root: Node) => {
  return d3
    .hierarchy<Node>(root)
    .descendants()
    .slice(1)
}

export const getAncList = (root: Node, startId: number, treeId: WhichTree) => {
  const h = d3.hierarchy<Node>(root)

  const nodes = h.descendants()

  let current

  if (treeId === WhichTree.Right) {
    current = nodes.find(
      x => x.data.id === startId && x.data.treeId === WhichTree.Right
    )!
  } else {
    current = nodes.find(
      x => x.data.id === startId && x.data.treeId !== WhichTree.Right
    )!
  }

  return current.ancestors()
}

export const loadDiff = async (
  paths: string[],
  maps: MyMap[],
  diffPoint: number[],
  nimServerPort: number
) => {
  for (let i = 0; i < maps.length; i++) {
    let ancestors = await fetchAncestors(paths[i], diffPoint[i], nimServerPort)

    maps[i] = insertNodesBoyo(ancestors, maps[i], WhichTree.Both)

    let treeId = i === 0 ? WhichTree.Left : WhichTree.Right

    getDescList(maps[i][diffPoint[i]]).forEach(x => {
      if (x.data.id !== diffPoint[i]) {
        x.data.treeId = treeId
      }
    })
  }

  return maps
}

export const loadAllDiffs = async (
  paths: string[],
  cores: Core[],
  diffLocations: number[][],
  nimServerPort: number
) => {
  let maps = [makeState(cores[0], 0).id2Node, makeState(cores[1], 0).id2Node]

  for (const array of diffLocations) {
    maps = await loadDiff(paths, maps, array, nimServerPort)
  }
  return maps
}

export const mergeMaps = (
  l: MyMap,
  r: MyMap,
  diffLocations: number[][],
  augmentedIds: number[][]
) => {
  let leftMap = cloneDeep(l)
  let rightMap = cloneDeep(r)

  if (isEqual(diffLocations, [[-1, -1]])) {
    const newRoot = new Node(-1, "", "", -2, 0, true, 2, false)
    newRoot.children = [leftMap[0], rightMap[0]]
    return { 0: newRoot }
  }

  for (const array of diffLocations) {
    if (
      leftMap[array[0]] &&
      rightMap[array[1]] &&
      rightMap[array[1]].children
    ) {
      if (!leftMap[array[0]].children) {
        leftMap[array[0]].children = []
      }
      leftMap[array[0]].children = leftMap[array[0]].children!.concat(
        rightMap[array[1]].children!
      )
    }
  }

  augmentedIds[0].forEach(augId => {
    if (!(augId in leftMap)) {
      return
    }
    let node = leftMap[augId]
    node.treeId = WhichTree.Left
  })

  augmentedIds[1].forEach(augId => {
    if (!(augId in rightMap)) {
      return
    }
    let node = rightMap[augId]
    node.treeId = WhichTree.Right

    if (!leftMap[node.parentId].children) {
      leftMap[node.parentId].children = []
    }

    leftMap[node.parentId].children!.push(node)
  })

  return leftMap
}
