import { makeState, insertNodesBoyo } from "./TreeHelper"
import Node, { WhichTree } from "./Node"
import { Core, MyMap } from "../components/vis/TreeContainer"
import { fetchAncestors } from "./MovementHelper"
import { isEqual, cloneDeep } from "lodash"
import * as d3 from "d3"
import { array } from "yup"
import { DiffPoint } from "../components/Forest"

export const assignTreeIds = (
  leftMap: MyMap,
  rightMap: MyMap,
  diffPoints: DiffPoint[]
) => {
  // if (isEqual(diffPoints, [[-1, -1]])) {
  //   leftMap[0].treeId = WhichTree.Left
  //   rightMap[0].treeId = WhichTree.Right
  //   getDescList(leftMap[0]).forEach(x => (x.data.treeId = WhichTree.Left))
  //   getDescList(rightMap[0]).forEach(x => (x.data.treeId = WhichTree.Right))
  //   return
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
  diffPoint: DiffPoint,
  nimServerPort: number
) => {
  for (let i = 0; i < maps.length; i++) {
    let diffPointId = i === 0 ? diffPoint.leftTreeId : diffPoint.rightTreeId

    let ancestors = await fetchAncestors(paths[i], diffPointId, nimServerPort)

    maps[i] = insertNodesBoyo(ancestors, maps[i], WhichTree.Both)

    // let treeId = i === 0 ? WhichTree.Left : WhichTree.Right

    // getDescList(maps[i][diffPointId]).forEach(x => {
    //   if (x.data.id !== diffPointId) {
    //     x.data.treeId = treeId
    //   }
    // })
  }

  return maps
}

export const loadAllDiffs = async (
  paths: string[],
  cores: Core[],
  diffPoints: DiffPoint[],
  nimServerPort: number
) => {
  let maps = [makeState(cores[0], 0).id2Node, makeState(cores[1], 0).id2Node]

  for (const diffPoint of diffPoints) {
    maps = await loadDiff(paths, maps, diffPoint, nimServerPort)
  }
  return maps
}

export const mergeMaps = (l: MyMap, r: MyMap, diffPoints: DiffPoint[]) => {
  let leftMap = cloneDeep(l)
  let rightMap = cloneDeep(r)

  // if (isEqual(diffPoints, [[-1, -1]])) {
  //   const newRoot = new Node(-1, "", "", -2, 0, true, 2, false)
  //   newRoot.children = [leftMap[0], rightMap[0]]
  //   return { 0: newRoot }
  // }

  for (const diffPoint of diffPoints) {
    diffPoint.highlightLeft.forEach(nodeId => {
      if (leftMap[nodeId]) {
        leftMap[nodeId].treeId = WhichTree.Left
      }
    })

    if (leftMap[diffPoint.leftTreeId]) {
      diffPoint.highlightRight.forEach(nodeId => {
        if (rightMap[nodeId]) {
          rightMap[nodeId].treeId = WhichTree.Right
          leftMap[diffPoint.leftTreeId].children!.push(rightMap[nodeId])
        }
      })
    }
  }

  return leftMap
}
