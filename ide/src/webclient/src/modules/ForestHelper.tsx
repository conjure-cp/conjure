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
    maps[i] = insertNodesBoyo(ancestors, maps[i])
  }

  return maps
}

export const mergeMaps = (
  l: MyMap,
  r: MyMap,
  diffLocations: number[][]
) => {

  let leftMap = cloneDeep(l)
  let rightMap = cloneDeep(r)

  if (isEqual(diffLocations, [[0, 0]])) {
    getNodeList(leftMap[0]).map(x => (x.data.treeID = WhichTree.Left))
    getNodeList(rightMap[0]).map(x => (x.data.treeID = WhichTree.Right))
    const newRoot = new Node(-1, "", "", -2, 0, true, 2, false)
    newRoot.children = [leftMap[0], rightMap[0]]
    return { 0: newRoot }
  }

  diffLocations.forEach(array => {
    if (!leftMap[array[0]].children) {
      return
    }

    leftMap[array[0]].children!.forEach(kid => {
      kid.treeID = WhichTree.Left
    })
  })

  diffLocations.forEach(array => {
    if (!rightMap[array[1]].children) {
      return
    }

    rightMap[array[1]].children!.forEach(kid => {
      kid.treeID = WhichTree.Right
    })
  })

  for (const array of diffLocations) {
    if (rightMap[array[1]].children && leftMap[array[0]].children!) {
      leftMap[array[0]].children = leftMap[array[0]].children!.concat(
        rightMap[array[1]].children!
      )
    }
  }

  // console.log(leftMap)

  return leftMap
}
