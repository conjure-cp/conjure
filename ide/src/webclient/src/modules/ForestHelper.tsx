import { makeState, insertNodesIntoMap } from "./TreeHelper"
import { Tree } from "../components/Forest"
import Node from "./Node"
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
  return maps
}

export const mergeMaps = (
  leftMap: MyMap,
  rightMap: MyMap,
  diffLocations: number[][]
) => {
  Object.keys(rightMap).forEach((key: any) => {
    rightMap[key].isLeftTree = false
  })
  for (const array of diffLocations) {
    leftMap[array[0]].children = leftMap[array[0]].children!.concat(
      rightMap[array[1]].children!
    )
  }

  console.log(leftMap[3])
}
// All nodes are isLeftTree === true by default..
