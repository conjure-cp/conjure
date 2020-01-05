import { HierarchyPointNode, select } from 'd3'
import * as d3 from 'd3'
import Node, { WhichTree } from './Node'
import { State, MyMap, Core, TreeContainer, FromServerNode } from '../components/vis/TreeContainer'
import { cloneDeep, last } from 'lodash'

export const linkGenerator = d3
	.linkVertical<any, HierarchyPointNode<Node>>()
	.x((d) => {
		return d.x
	})
	.y((d) => {
		return d.y
	})

export const showAllAncestorsBoyo = (map: MyMap, startId: number): MyMap => {
	let newMap = cloneDeep(map)

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

export const makeState = (core: Core, selected: number): State => {
	const minsize = 7
	const solveable = core.nodes.find((n) => n.isSolution) !== undefined
	const linScale = d3
		.scaleLinear()
		.domain([ 0, core.nodes[0].descCount ]) // upper domain is Way more just to be safe
		.range([ minsize, 30 ])

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
			element.isSolution,
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

	let totalNodeCount = last(core.solAncestorIds)! + 1

	let state: State = {
		id2Node: id2Node,
		minsize: minsize,
		solveable: solveable,
		linScale: linScale,
		selected: selected,
		shouldGetKids: false,
		solNodeIds: solNodeIds,
		totalNodeCount: totalNodeCount,
		failedBranchCount: totalNodeCount - (solveable ? core.solAncestorIds.length : 0),
	}

	return state
}

export const sleep = (ms: number) => {
	return new Promise((resolve) => setTimeout(resolve, ms))
}

export const insertNodesBoyo = (nodes: FromServerNode[], map: MyMap, treeId: WhichTree): MyMap => {
	// console.log(JSON.stringify(nodes))

	nodes.map((node: FromServerNode) => {
		const newNode = new Node(
			node.id,
			node.label,
			node.prettyLabel,
			node.parentId,
			node.descCount,
			node.isLeftChild,
			node.childCount,
			node.isSolution,
		)

		newNode.treeId = treeId

		// console.log(newNode.parentId)

		if (map[newNode.parentId] && map[newNode.parentId]._children) {
			Node.showChildren(map[newNode.parentId])
		}

		if (newNode.id in map) {
			return
		}

		if (!map[newNode.parentId].children) {
			map[newNode.parentId].children = []
		}

		if (newNode.isLeftChild) {
			map[newNode.parentId].children!.unshift(newNode)
		} else {
			map[newNode.parentId].children!.push(newNode)
		}

		map[newNode.id] = newNode
	})
	return map
}

export const insertNodes = (nodes: Node[], nextId: number, instance: TreeContainer) => {
	instance.setState((prevState: State) => {
		let newMap = cloneDeep(prevState.id2Node)
		return {
			id2Node: insertNodesBoyo(nodes, newMap, WhichTree.Both),
			selected: nextId,
		}
	})
}

export const requestServer = async (url: string, payload: string | null, nim: boolean) => {
	const reqInfo = {
		method: payload ? 'POST' : 'GET',
		headers: {
			Accept: 'application/json, text/plain, */*',
			'Content-Type': 'application/json',
		},
		body: payload ? JSON.stringify(payload) : null,
	}

	try {
		const res = await fetch(url, reqInfo)
		const json = await res.json()
		return json
	} catch (error) {
		return {
			stackTrace: error.message,
			message: `Failed to make request to ${nim ? 'nim' : 'vscode'} server.`,
			url: url,
			reqInfo: reqInfo,
		}
	}
}
