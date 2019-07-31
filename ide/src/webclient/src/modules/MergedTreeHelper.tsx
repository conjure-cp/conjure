import { MyMap } from "../components/vis/MergedTreeContainer"
import { getAncList, mergeMaps, getDescList, loadDiff } from "./ForestHelper"
import { WhichTree } from "./Node"
import { goLeftBoyo, goRightBoyo } from "./MovementHelper"
import { DiffPoint } from "../components/Forest"
import { min, max, maxBy } from "lodash"
import { findDOMNode } from "react-dom"

const getDiffPointKids = (
  leftMap: MyMap,
  rightMap: MyMap,
  currentSelected: number,
  currentTreeId: number,
  diffPoints: DiffPoint[]
) => {
  let leftDiffIds = diffPoints.map(x => x.leftTreeId)
  let rightDiffIds = diffPoints.map(x => x.rightTreeId)
  let index = -1

  if (
    currentTreeId !== WhichTree.Right &&
    leftDiffIds.includes(currentSelected)
  ) {
    index = leftDiffIds.indexOf(currentSelected)
  }

  if (index !== -1) {
    let leftDiffPoint = leftDiffIds[index]
    let rightDiffPoint = rightDiffIds[index]
    let leftNode = leftMap[leftDiffPoint]
    let rightNode = rightMap[rightDiffPoint]

    // one of the diffpoint  nodes haven't been loaded yet

    if (!leftNode || !rightNode) {
      return undefined
    }

    if (!leftNode.children) {
      leftNode.children = []
    }
    if (!rightNode.children) {
      rightNode.children = []
    }

    let combined = leftMap[leftDiffPoint].children!.concat(
      rightMap[rightDiffPoint].children!.filter(x =>
        diffPoints[index].highlightRight.includes(x.id)
      )
    )

    return combined.length > 0 ? combined : undefined
  }
  return undefined
}

export const goRightMerged = async (
  leftMap: MyMap,
  rightMap: MyMap,
  currentSelected: number,
  currentTreeId: number,
  diffPoints: DiffPoint[]
) => {
  let diffPointKids = getDiffPointKids(
    leftMap,
    rightMap,
    currentSelected,
    currentTreeId,
    diffPoints
  )
  if (diffPointKids) {
    // console.log(kids)

    if (diffPointKids.length < 2) {
      return {
        selected: diffPointKids[0].id,
        selectedTreeId: diffPointKids[0].treeId
      }
    }

    if (diffPointKids.length < 3) {
      return {
        selected: diffPointKids[1].id,
        selectedTreeId: diffPointKids[1].treeId
      }
    }

    return {
      selected: diffPointKids[2].id,
      selectedTreeId: diffPointKids[2].treeId
    }
  }

  let map = currentTreeId === WhichTree.Right ? rightMap : leftMap

  return {
    ...goRightBoyo(map, currentSelected),
    selectedTreeId: currentTreeId
  }
}
export const goDownMerged = async (
  leftMap: MyMap,
  rightMap: MyMap,
  currentSelected: number,
  currentTreeId: number,
  diffPoints: DiffPoint[],
  leftPath: string,
  rightPath: string,
  nimServerPort: number,
) => {
  let kids = getDiffPointKids(
    leftMap,
    rightMap,
    currentSelected,
    currentTreeId,
    diffPoints
  )

  if (kids) {
    if (kids.length < 1) {
      return {
        selected: currentSelected,
        selectedTreeId: currentTreeId,
        leftMap,
        rightMap
      }
    }

    if (kids.length <= 2) {
      return {
        selected: kids[0].id,
        selectedTreeId: kids[0].treeId,
        leftMap,
        rightMap
      }
    }

    return {
      selected: kids[1].id,
      selectedTreeId: kids[1].treeId,
      leftMap,
      rightMap
    }
  }

  return await goLeftMerged(
    currentSelected,
    currentTreeId,
    leftPath,
    rightPath,
    leftMap,
    rightMap,
    diffPoints,
    nimServerPort,
  )
}

export const goUpMerged = (
  leftMap: MyMap,
  rightMap: MyMap,
  currentSelected: number,
  currentTreeId: number,
  diffPoints: DiffPoint[]
) => {
  let leftDiffIds = diffPoints.map(x => x.leftTreeId)
  let rightDiffIds = diffPoints.map(x => x.rightTreeId)
  let currentNode
  let nextId

  if (currentTreeId === WhichTree.Right) {
    currentNode = rightMap[currentSelected]
    nextId = currentNode.parentId
    if (rightDiffIds.includes(currentNode.parentId)) {
      currentTreeId = WhichTree.Both
      nextId = leftDiffIds[rightDiffIds.indexOf(nextId)]
    }
  } else {
    currentNode = leftMap[currentSelected]
    nextId = currentNode.parentId
    if (leftDiffIds.includes(currentNode.parentId)) {
      currentTreeId = WhichTree.Both
    }
  }

  return {
    selected: nextId,
    selectedTreeId: currentTreeId
  }
}

export const goLeftAtDiffingPoint = async (
  leftMap: MyMap,
  rightMap: MyMap,
  currentSelected: number,
  diffPoints: DiffPoint[],
  leftPath: string,
  rightPath: string,
  nimServerPort: number
) => {
  const leftDiffIds = diffPoints.map(x => x.leftTreeId)

  let index = leftDiffIds.indexOf(currentSelected)

  let maps = [leftMap, rightMap]

  maps = await loadDiff(
    [leftPath, rightPath],
    [leftMap, rightMap],
    diffPoints[index],
    nimServerPort
  )

  let mergedMap = mergeMaps(maps[0], maps[1], diffPoints)
  let currentNode = mergedMap[currentSelected]
  let nextNode = currentNode.children![0]

  return {
    selected: nextNode.id,
    selectedTreeId: nextNode.treeId,
    leftMap: maps[0],
    rightMap: maps[1]
  }
}

export const reviseGoLeft = (
  mergedMap: MyMap,
  currentSelected: number,
  treeId: WhichTree,
  diffPoints: DiffPoint[]
) => {
  let ancestors = getAncList(mergedMap[0], currentSelected, treeId)
  let ancestorIds = ancestors.map(y => y.id)
  let diffAncs = diffPoints.filter(
    x =>
      ancestorIds.includes(x.leftTreeId) &&
      ancestors.find(y => y.id === x.leftTreeId)!.treeId === WhichTree.Both
  )

  let diffPoint = maxBy(diffAncs, x => x.leftTreeId)!

  ancestorIds = getAncList(
    mergedMap[0],
    diffPoint!.leftTreeId,
    WhichTree.Both
  ).map(y => y.id)

  let diffPointIndex = ancestorIds.indexOf(diffPoint.leftTreeId)

  let currentNode = mergedMap[ancestorIds[diffPointIndex - 1]]

  // let currentIndex = 0
  // let currentNode = mergedMap[ancestorIds[currentIndex]]

  // while (
  //   currentNode.children!.length < 2 ||
  //   ancestorIds.includes(currentNode.children![1].id)
  // ) {
  //   currentIndex++
  //   currentNode = mergedMap[ancestorIds[currentIndex]]
  //   if (!currentNode) {
  //     return { selected: currentSelected, treeId: treeId }
  //   }
  // }

  let id = currentNode.children![1].id
  return {
    selected: id,
    treeId: currentNode.children![1].treeId
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
  diffPoints: DiffPoint[],
  nimServerPort: number,
): Promise<{
  selected: number
  selectedTreeId: number
  leftMap: MyMap
  rightMap: MyMap
}> => {
  let leftDiffIds = diffPoints.map(x => x.leftTreeId)
  if (
    leftDiffIds.includes(currentSelected) &&
    currentTreeId !== WhichTree.Right
  ) {
    return {
      ...(await goLeftAtDiffingPoint(
        leftMap,
        rightMap,
        currentSelected,
        diffPoints,
        leftPath,
        rightPath,
        nimServerPort,
      )),
      leftMap,
      rightMap
    }
  }

  let isRightTree = currentTreeId === WhichTree.Right

  let path = leftPath
  let map = leftMap
  let nextTreeId = currentTreeId

  if (isRightTree) {
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
    1,
    nimServerPort,
    nextTreeId
  )

  let nextSelected = res.selected

  let mergedMap = mergeMaps(leftMap, rightMap, diffPoints)

  if (isRightTree) {
    rightMap = res.id2Node
  } else {
    leftMap = res.id2Node
    if (
      getDescList(leftMap[nextSelected]).filter(x => leftDiffIds.includes(x.id))
        .length > 0
    ) {
      nextTreeId = WhichTree.Both
    }
  }

  if (isRightTree) {
    if (shouldBeRightTree(leftMap, rightMap, nextSelected, isRightTree)) {
      nextTreeId = WhichTree.Right
    } else {
      let revision = reviseGoLeft(
        mergedMap,
        currentSelected,
        nextTreeId,
        diffPoints
      )
      nextSelected = revision.selected
      nextTreeId = revision.treeId
    }
  } else {
    // We may have moved to a both node
    if (
      !getAncList(leftMap[0], nextSelected, currentTreeId).find(
        x => x.treeId === currentTreeId
      )
    ) {
      nextTreeId = WhichTree.Both
    }
  }

  return {
    selected: nextSelected,
    selectedTreeId: nextTreeId,
    leftMap: leftMap,
    rightMap: rightMap
  }
}
