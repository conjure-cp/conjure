import { MyMap } from "../components/vis/MergedTreeContainer"
import { getAncList, mergeMaps, getDescList } from "./ForestHelper"
import { WhichTree } from "./Node"
import { goLeftBoyo } from "./MovementHelper"

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
  treeId: WhichTree,
  diffLocations: number[][]
) => {
  let ancestorIds = getAncList(mergedMap[0], currentSelected, treeId).map(
    y => y.data.id
  )
  let diffPoint = diffLocations.find(x => ancestorIds.includes(x[0]))
  //   console.log(diffPoint)

  ancestorIds = getAncList(mergedMap[0], diffPoint![0], WhichTree.Both).map(
    y => y.data.id
  )
  //   console.log(ancestorIds)

  let currentIndex = 0
  let currentNode = mergedMap[ancestorIds[currentIndex]]

  //   console.log(currentNode)
  while (
    diffLocations.map(x => x[0]).includes(currentNode.id) ||
    currentNode.children!.length < 2 ||
    ancestorIds.includes(currentNode.children![1].id)
  ) {
    // console.log(currentNode.id)
    // console.log(currentNode)
    currentIndex++
    currentNode = mergedMap[ancestorIds[currentIndex]]
    if (!currentNode) {
      return { selected: currentSelected, treeId: treeId }
    }
  }

  let id = currentNode.children![1].id
  return {
    selected: id,
    treeId: WhichTree.Both
  }
}

export const shouldBeRightTree = (
  leftMap: MyMap,
  rightMap: MyMap,
  nextSelected: number,
  isRightTree: boolean
): boolean => {
  if (!isRightTree) {
    return false
  }

  if (
    leftMap[nextSelected] &&
    rightMap[nextSelected] &&
    rightMap[nextSelected].treeId !== WhichTree.Both
  ) {
    return true
  }

  // If it aint in the left or both but is in the right then go right
  if (
    !leftMap[nextSelected] &&
    rightMap[nextSelected] &&
    rightMap[nextSelected].treeId === WhichTree.Right
  ) {
    return true
  }

  return false
}

export const goLeftMerged = async (
  currentSelected: number,
  currentTreeId: number,
  leftPath: string,
  rightPath: string,
  leftMap: MyMap,
  rightMap: MyMap,
  diffLocations: number[][],
  nimServerPort: number
): Promise<{ selected: number; selectedTreeId: number; mergedMap: MyMap }> => {
  let mergedMap = mergeMaps(leftMap, rightMap, diffLocations)
  let leftDiffIds = diffLocations.map(x => x[0])

  if (
    leftDiffIds.includes(currentSelected) &&
    currentTreeId !== WhichTree.Right
  ) {
    return { ...goLeftAtDiffingPoint(mergedMap, currentSelected), mergedMap }
  }

  let isRightTree = currentTreeId === WhichTree.Right

  let path = leftPath
  let map = leftMap
  let nextTreeId = currentTreeId

  if (isRightTree) {
    // if (this.props.diffLocations.selected === origState.selected) {
    path = rightPath
    map = rightMap
    nextTreeId = WhichTree.Right
  }

  let res = await goLeftBoyo(
    currentSelected,
    map!,
    false,
    false,
    path,
    10,
    nimServerPort,
    nextTreeId
  )

  let nextSelected = res.selected

  mergedMap = mergeMaps(leftMap, rightMap, diffLocations)

  if (isRightTree) {
    rightMap = res.id2Node
  } else {
    leftMap = res.id2Node
    if (
      getDescList(leftMap[nextSelected]).filter(x =>
        leftDiffIds.includes(x.data.id)
      ).length > 0
    ) {
      nextTreeId = WhichTree.Both
    }
  }

  if (isRightTree) {
    // If there is also a node with the same ID in the both section, then choose the one from the right tree

    if (shouldBeRightTree(leftMap, rightMap, nextSelected, isRightTree)) {
      nextTreeId = WhichTree.Right
    } else {
      let revision = reviseGoLeft(
        mergedMap,
        currentSelected,
        nextTreeId,
        diffLocations
      )
      nextSelected = revision.selected
      nextTreeId = revision.treeId
    }
  }

  return {
    selected: nextSelected,
    selectedTreeId: nextTreeId,
    mergedMap: mergedMap
  }
}
