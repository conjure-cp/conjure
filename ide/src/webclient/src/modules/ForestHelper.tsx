import { makeState, insertNodesBoyo } from "./TreeHelper"
import { Tree } from "../components/Forest"
import Node, { WhichTree } from "./Node"
import { Core, MyMap } from "../components/vis/TreeContainer"
import { fetchAncestors } from "./MovementHelper"
import { isTSImportEqualsDeclaration } from "@babel/types"
import { isEqual, cloneDeep } from "lodash"
import * as d3 from "d3"

export const getNodeList = (root: Node) => {
  const hierarchy = d3.hierarchy<Node>(root)
  const layout = d3.tree<Node>()
  const rootNode = layout(hierarchy)
  return rootNode.descendants()
}

export const getLeaves = (root: Node) => {
  const hierarchy = d3.hierarchy<Node>(root)
  const layout = d3.tree<Node>()
  const rootNode = layout(hierarchy)
  return rootNode.leaves()
}

export const loadDiffs = async (
  paths: string[],
  cores: Core[],
  diffLocations: number[][],
  nimServerPort: number
) => {
  const maps = [makeState(cores[0], 0).id2Node, makeState(cores[1], 0).id2Node]

  for (let i = 0; i < maps.length; i++) {
    let ancestors: Node[] = []
    for (const array of diffLocations) {
      ancestors = ancestors.concat(
        await fetchAncestors(paths[i], array[i], nimServerPort)
      )
    }
    maps[i] = insertNodesBoyo(ancestors, maps[i], WhichTree.Both)
  }

  return maps
}

export const mergeMaps = (l: MyMap, r: MyMap, diffLocations: number[][]) => {
  let leftMap = cloneDeep(l)
  let rightMap = cloneDeep(r)

  if (isEqual(diffLocations, [[0, 0]])) {
    getNodeList(leftMap[0]).forEach(x => (x.data.treeId = WhichTree.Left))
    getNodeList(rightMap[0]).forEach(x => (x.data.treeId = WhichTree.Right))
    const newRoot = new Node(-1, "", "", -2, 0, true, 2, false)
    newRoot.children = [leftMap[0], rightMap[0]]
    return { 0: newRoot }
  }

  diffLocations.forEach(array => {
    getNodeList(leftMap[array[0]]).forEach(x => {
      x.data.treeId = WhichTree.Left
    })
  })

  diffLocations.forEach(array => {
    getNodeList(rightMap[array[1]]).forEach(x => {
      x.data.treeId = WhichTree.Right
    })
  })

  for (const array of diffLocations) {
    if (rightMap[array[1]].children) {
      if (!leftMap[array[0]].children) {
        leftMap[array[0]].children = []
      }
      leftMap[array[0]].children = leftMap[array[0]].children!.concat(
        rightMap[array[1]].children!
      )
    }
  }

  let map: MyMap = {}
  getNodeList(leftMap[0]).forEach((x, i) => {
    x.data.newId = i
    map[i] = x.data
  })

  // console.log(map)

  // return leftMap
  return map
}
