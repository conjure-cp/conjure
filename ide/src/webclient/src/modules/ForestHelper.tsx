import { makeState, insertNodesBoyo } from "./TreeHelper"
import Node, { WhichTree } from "./Node"
import { Core, MyMap } from "../components/vis/TreeContainer"
import { fetchAncestors } from "./MovementHelper"
import { isEqual, cloneDeep } from "lodash"
import * as d3 from "d3"

export const getDescList = (root: Node) => {
  return d3.hierarchy<Node>(root).descendants()
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
  augmentedIds: number[]
) => {
  const lIsBigger = l[0].descCount >= r[0].descCount

  let leftMap = cloneDeep(l)
  let rightMap = cloneDeep(r)

  if (isEqual(diffLocations, [[-1, -1]])) {
    getDescList(leftMap[0]).forEach(x => (x.data.treeId = WhichTree.Left))
    getDescList(rightMap[0]).forEach(x => (x.data.treeId = WhichTree.Right))
    const newRoot = new Node(-1, "", "", -2, 0, true, 2, false)
    newRoot.children = [leftMap[0], rightMap[0]]
    return { 0: newRoot }
  }

  diffLocations.forEach(array => {
    if (!leftMap[array[0]]) {
      return
    }

    getDescList(leftMap[array[0]])
      .filter(x => x.data.id !== array[0])
      .forEach(x => {
        x.data.treeId = WhichTree.Left
      })
  })

  diffLocations.forEach(array => {
    if (!rightMap[array[1]]) {
      return
    }

    getDescList(rightMap[array[1]])
      .filter(x => x.data.id !== array[1])
      .forEach(x => {
        x.data.treeId = WhichTree.Right
      })
  })

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

  if (lIsBigger && diffLocations[0][0] === rightMap[0].descCount) {
    getDescList(leftMap[0])
      .filter(x => x.data.id > rightMap[0].descCount)
      .forEach(x => {
        x.data.treeId = WhichTree.Left
      })
  } else {
    augmentedIds.forEach(id => {
      let node = rightMap[id]
      node.treeId = WhichTree.Right
      leftMap[node.parentId].children!.push(node)
    })
  }

  return leftMap
}
