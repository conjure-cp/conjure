import { HierarchyPointNode } from "d3"
import * as d3 from "d3"
import Node from "./Node"
import { State, MyMap, Core, TreeContainer } from "../components/TreeContainer"
import { cloneDeep, last } from "lodash"

export const linkGenerator = d3
  .linkVertical<any, HierarchyPointNode<Node>>()
  .x(d => {
    return d.x
  })
  .y(d => {
    return d.y
  })

export const showAllAncestors = (prevState: State, startId: number): MyMap => {
  let newMap = cloneDeep(prevState.id2Node)

  let currentId = newMap[startId].id

  while (true) {
    Node.showChildren(newMap[currentId])
    if (currentId === 0) {
      break
    }
    currentId = newMap[newMap[currentId].parentId].id
  }

  return newMap
}

export const getNextSolId = (prevState: State): number => {
  if (!prevState.solveable) {
    return prevState.selected
  }

  const currentIdInSolNodeIds = prevState.solNodeIds.indexOf(prevState.selected)

  if (currentIdInSolNodeIds === -1) {
    return prevState.solNodeIds[0]
  }

  if (prevState.solNodeIds.length <= currentIdInSolNodeIds + 1) {
    return -1
  }

  return prevState.solNodeIds[currentIdInSolNodeIds + 1]
}

export const getPrevFailedId = (current: number, solAncestorIds: number[]) => {
  let counter = current

  while (solAncestorIds.includes(counter) && counter > 0) {
    counter--
  }

  if (!solAncestorIds.includes(counter)) {
    return counter
  }

  return -1
}

export const getNextFailedId = (current: number, solAncestorIds: number[]) => {
  let counter = current

  while (solAncestorIds.includes(counter) && counter < last(solAncestorIds)!) {
    counter++
  }

  if (!solAncestorIds.includes(counter)) {
    return counter
  }

  return -1
}

export const getPrevSolId = (prevState: State): number => {
  if (!prevState.solveable) {
    return prevState.selected
  }

  const currentIdInSolNodeIds = prevState.solNodeIds.indexOf(prevState.selected)

  if (currentIdInSolNodeIds === -1) {
    return last(prevState.solNodeIds)!
  }

  if (currentIdInSolNodeIds - 1 < 0) {
    return -1
  }

  return prevState.solNodeIds[currentIdInSolNodeIds - 1]
}

export const makeState = (core: Core): State => {
  const minsize = 7
  const solveable = core.nodes.find(n => n.isSolution) !== undefined
  const linScale = d3
    .scaleLinear()
    .domain([0, core.nodes[0].descCount]) // upper domain is Way more just to be safe
    .range([minsize, 30])

  let id2Node: MyMap = {}

  let solNodeIds = []

  for (let i = 0; i < core.nodes.length; i++) {
    const element = core.nodes[i]

    if (element.isSolution) {
      solNodeIds.push(element.id)
    }

    const newNode = new Node(
      element.id,
      element.label,
      element.prettyLabel,
      element.parentId,
      element.descCount,
      element.isLeftChild,
      element.childCount,
      element.isSolution
    )

    const parentId = newNode.parentId
    if (newNode.parentId === -1) {
      id2Node[newNode.id] = newNode
      continue
    }
    if (!id2Node[parentId].children) {
      id2Node[parentId].children = []
    }
    if (newNode.isLeftChild) {
      id2Node[parentId].children!.unshift(newNode)
    } else {
      id2Node[parentId].children!.push(newNode)
    }
    id2Node[newNode.id] = newNode
  }

  let state: State = {
    id2Node: id2Node,
    minsize: minsize,
    solveable: solveable,
    linScale: linScale,
    selected: 0,
    shouldGetKids: false,
    solNodeIds: solNodeIds,
    totalNodeCount: last(core.solAncestorIds)! + 1
  }

  return state
}

export const sleep = (ms: number) => {
  return new Promise(resolve => setTimeout(resolve, ms))
}

export const insertNodes = (
  nodes: Node[],
  nextId: number,
  instance: TreeContainer
) => {
  instance.setState((prevState: State) => {
    let newMap = cloneDeep(prevState.id2Node)
    nodes.map((node: Node) => {
      if (node.id in newMap) {
        return
      }

      if (!newMap[node.parentId].children) {
        newMap[node.parentId].children = []
      }
      if (node.isLeftChild) {
        newMap[node.parentId].children!.unshift(node)
      } else {
        newMap[node.parentId].children!.push(node)
      }

      newMap[node.id] = node
    })
    return { id2Node: newMap, selected: nextId }
  })
}
