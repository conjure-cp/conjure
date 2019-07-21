import { MyMap } from "../components/vis/MergedTreeContainer"
import { getAncList } from "./ForestHelper"
import { WhichTree } from "./Node"

export const goLeftAtDiffingPoint = (
  mergedMap: MyMap,
  currentSelected: number
) => {
  let currentNode = mergedMap[currentSelected]
  let nextNode = currentNode.children![0]

  return {
    selected: nextNode.id,
    selectedTreeId: nextNode.treeId
  }
}

export const reviseGoLeft = (
  mergedMap: MyMap,
  currentSelected: number,
  newSelected: number,
  treeId: WhichTree,
  diffLocations: number[][]
) => {
  let ancestorIds = getAncList(mergedMap[0], currentSelected, treeId).map(
    y => y.data.id
  )

  let aboveDiffPoint = diffLocations.find(x => ancestorIds.includes(x[0]))

  // Near the end of the tree
  if (!aboveDiffPoint) {
    return { selected: newSelected, treeId }
  }

  const nextNode = mergedMap[aboveDiffPoint[0] - 1].children![1]

  if (!nextNode) {
    return { selected: currentSelected, treeId }
  }

  return {
    selected: nextNode.id,
    treeId: WhichTree.Both
  }
}

export const shouldBeRightTree = (
  leftMap: MyMap,
  rightMap: MyMap,
  nextSelected: number,
  isRightTree: boolean,
  diffLocations: number[][]
): boolean => {
  if (
    leftMap[nextSelected] &&
    leftMap[nextSelected].treeId === WhichTree.Both &&
    isRightTree
  ) {
    return true
  }

  let rightDiffIds = diffLocations.map(x => x[1])

  // If it aint in the left or both but is in the right then go right

  if (
    !leftMap[nextSelected] &&
    rightMap[nextSelected] &&
    isRightTree &&
    !rightDiffIds.includes(nextSelected + 1)
    // !getAncList(rightMap[0], nextSelected + 1, WhichTree.Right).includes()
  ) {
    return true
  }

  return false
}
