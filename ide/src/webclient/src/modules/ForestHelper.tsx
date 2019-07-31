import { makeState, insertNodesBoyo } from "./TreeHelper"
import Node, { WhichTree } from "./Node"
import { Core, MyMap } from "../components/vis/TreeContainer"
import { fetchAncestors, fetchDescendants } from "./MovementHelper"
import { isEqual, cloneDeep, sumBy, sum } from "lodash"
import * as d3 from "d3"
import { array } from "yup"
import { DiffPoint } from "../components/Forest"
import { listenerCount } from "cluster"

export const assignTreeIds = (
  leftMap: MyMap,
  rightMap: MyMap,
  diffPoints: DiffPoint[]
) => {
  if (diffPoints[0].leftTreeId === -1 && diffPoints[0].rightTreeId === -1) {
    leftMap[0].treeId = WhichTree.Left
    rightMap[0].treeId = WhichTree.Right
    getDescList(leftMap[0]).forEach(x => (x.treeId = WhichTree.Left))
    getDescList(rightMap[0]).forEach(x => (x.treeId = WhichTree.Right))
    return
  }

  diffPoints.forEach(diffPoint => {
    diffPoint.highlightLeft.forEach(leftId => {
      if (leftId in leftMap) {
        leftMap[leftId].treeId = WhichTree.Left

        let descList = getDescList(leftMap[leftId])
        for (const des of descList) {
          leftMap[des.id].treeId = WhichTree.Left
        }
      }
    })

    diffPoint.highlightRight.forEach(rightId => {
      if (rightId in rightMap) {
        rightMap[rightId].treeId = WhichTree.Right

        let descList = getDescList(rightMap[rightId])
        for (const des of descList) {
          rightMap[des.id].treeId = WhichTree.Right
        }
      }
    })
  })
}

export const getDescList = (root: Node) => {
  return d3
    .hierarchy<Node>(root)
    .descendants()
    .slice(1)
    .map(x => x.data)
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

  return current
    .ancestors()
    .map(x => x.data)
    .reverse()
    .slice(1)
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

    // maps[i] = await fetchDescendants(
    //   diffPointId,
    //   maps[i],
    //   paths[i],
    //   100,
    //   nimServerPort,
    //   WhichTree.Both
    // )

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

  assignTreeIds(leftMap, rightMap, diffPoints)

  if (diffPoints[0].leftTreeId === -1 && diffPoints[0].rightTreeId === -1) {
    const newRoot = new Node(-1, "", "", -2, 0, true, 2, false)
    newRoot.children = [leftMap[0], rightMap[0]]
    return { 0: newRoot }
  }

  for (const diffPoint of cloneDeep(diffPoints).reverse()) {
    if (leftMap[diffPoint.leftTreeId]) {
      diffPoint.highlightRight.forEach(nodeId => {
        if (rightMap[nodeId]) {
          if (!leftMap[diffPoint.leftTreeId].children) {
            leftMap[diffPoint.leftTreeId].children = []
          }

          leftMap[diffPoint.leftTreeId].children!.push(rightMap[nodeId])
        }
      })

      leftMap[diffPoint.leftTreeId].descCount = diffPoint.descCount
      leftMap[diffPoint.leftTreeId].childCount = 2
      //   sumBy(leftMap[diffPoint.leftTreeId].children, x => x.descCount) +
      //   diffPoint.highlightRight.length +
      //   leftMap[diffPoint.leftTreeId].childCount

      console.log("-----")
      console.log(diffPoint.leftTreeId)
      console.log(diffPoint.descCount)
      console.log(leftMap[diffPoint.leftTreeId].descCount)
      // console.log(
      //   sumBy(leftMap[diffPoint.leftTreeId].children, x => x.descCount) +
      //     sumBy(diffPoint.highlightRight.map(x => rightMap[x]), x => x.descCount) +
      //     leftMap[diffPoint.leftTreeId].childCount
      // )
      // console.log(diffPoint)
    }
  }

  // function recurse(node: Node) {
  //   if (node.children) {
  //     node.children.forEach(x => recurse(x))
  //     node.descCount = sumBy(node.children, x => x.descCount) + node.children.length
  //     console.log("-----")
  //     console.log(node.id)
  //     console.log(node.descCount)
  //   }
  //   // node.descCount = sumBy(node.children, x => x.descCount)
  // }

  // recurse(leftMap[0])

  // console.log(getDescList(leftMap[0]))

  return leftMap
}
