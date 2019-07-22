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
  let diffPoint = diffLocations.find(x => ancestorIds.includes(x[0]))
  console.log(diffPoint)

  ancestorIds = getAncList(mergedMap[0], diffPoint![0], WhichTree.Both).map(
    y => y.data.id
  )
  console.log(ancestorIds)

  let currentIndex = 0
  let currentNode = mergedMap[ancestorIds[currentIndex]]

  console.log(currentNode)
  while (
    diffLocations.map(x => x[0]).includes(currentNode.id) ||
    currentNode.children!.length < 2 ||
    ancestorIds.includes(currentNode.children![1].id)
  ) {
    console.log(currentNode.id)
    console.log(currentNode)
    if (!currentNode) {
      return { selected: currentSelected, treeId: treeId }
    }
    currentIndex++
    currentNode = mergedMap[ancestorIds[currentIndex]]
  }

  let id = currentNode.children![1].id
  return {
    selected: id,
    treeId: WhichTree.Both
  }

  //---------------------------------------------
  //   let ancestorIds = getAncList(mergedMap[0], currentSelected, treeId).map(
  //     y => y.data.id
  //   )
  //   let aboveDiffPoint = diffLocations.find(x => ancestorIds.includes(x[0]))

  //   //   Near the end of the tree
  //   if (!aboveDiffPoint) {
  //     return { selected: newSelected, treeId }
  //   }

  //   const nextNode = mergedMap[aboveDiffPoint[0] - 1].children![1]

  //   if (!nextNode) {
  //     return { selected: currentSelected, treeId }
  //   }

  //   console.log("here")
  //   return {
  //     selected: nextNode.id,
  //     treeId: WhichTree.Both
  //   }
}

export const shouldBeRightTree = (
  leftMap: MyMap,
  rightMap: MyMap,
  nextSelected: number,
  isRightTree: boolean,
  diffLocations: number[][]
): boolean => {
  let rightDiffIds = diffLocations.map(x => x[1])
  if (!isRightTree) {
    return false
  }

  //   if (
  //     rightMap[nextSelected] &&
  //     rightMap[nextSelected].treeId === WhichTree.Both &&
  //     rightDiffIds.includes(rightMap[nextSelected].parentId)
  //   ) {
  //     return false
  //   }

  //   console.log("here")
  if (
    leftMap[nextSelected] &&
    rightMap[nextSelected] &&
    rightMap[nextSelected].treeId !== WhichTree.Both
    // leftMap[nextSelected].treeId === WhichTree.Left
  ) {
    return true
  }

  // If it aint in the left or both but is in the right then go right

  if (
    !leftMap[nextSelected] &&
    rightMap[nextSelected] &&
    rightMap[nextSelected].treeId === WhichTree.Right
    // !rightDiffIds.includes(nextSelected + 1)
  ) {
    return true
  }

  return false
}
