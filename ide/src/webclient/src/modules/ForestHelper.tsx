import { makeState, insertNodesIntoMap } from "./TreeHelper"
import { Tree } from "../components/Forest"
import Node, { WhichTree } from "./Node"
import { Core, MyMap } from "../components/vis/TreeContainer"
import { fetchAncestors } from "./MovementHelper"

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
    maps[i] = insertNodesIntoMap(ancestors, maps[i])
  }

  // console.log(maps[2])

  return maps
}

export const mergeMaps = (
  leftMap: MyMap,
  rightMap: MyMap,
  diffLocations: number[][]
) => {
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
