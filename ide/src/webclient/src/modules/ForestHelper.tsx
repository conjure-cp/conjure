import { makeState, insertNodesBoyo } from "./TreeHelper"
import { Tree } from "../components/Forest"
import Node, { WhichTree } from "./Node"
import { Core, MyMap } from "../components/vis/TreeContainer"
import { fetchAncestors } from "./MovementHelper"
import { isTSImportEqualsDeclaration } from "@babel/types"
import { isEqual, cloneDeep } from "lodash"
import * as d3 from "d3"

export const getDescList = (root: Node) => {
  return d3.hierarchy<Node>(root).descendants()
}

export const getAncList = (root: Node, startId: number, treeId: WhichTree) => {
  const h = d3.hierarchy<Node>(root)
  const nodes = h.descendants()
  const current = nodes.find(
    x => x.data.id === startId && x.data.treeId === treeId
  )!
  return current.ancestors()
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
    getDescList(leftMap[0]).forEach(x => (x.data.treeId = WhichTree.Left))
    getDescList(rightMap[0]).forEach(x => (x.data.treeId = WhichTree.Right))
    const newRoot = new Node(-1, "", "", -2, 0, true, 2, false)
    newRoot.children = [leftMap[0], rightMap[0]]
    return { 0: newRoot }
  }

  diffLocations.forEach(array => {
    getDescList(leftMap[array[0]])
      .filter(x => x.data.id !== array[0])
      .forEach(x => {
        x.data.treeId = WhichTree.Left
      })
  })

  diffLocations.forEach(array => {
    getDescList(rightMap[array[1]])
      .filter(x => x.data.id !== array[1])
      .forEach(x => {
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

  let counter = 0
  let map: MyMap = {}

  let recurse = (insideNode: Node) => {
    insideNode.newId = counter
    map[insideNode.newId] = insideNode
    counter++

    if (!insideNode.children) return

    insideNode.children!.forEach(kid => {
      recurse(kid)
    })
  }

  recurse(leftMap[0])
  // getNodeList(leftMap[0]).forEach((x, i) => {
  //   x.data.newId = x.value
  //   map[i] = x.data
  // })

  // console.log(map)

  return leftMap
  // return map
}
