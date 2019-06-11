import * as React from "react"
import * as ReactDOM from "react-dom"
import * as d3 from 'd3'
import Tree from 'react-d3-tree'
import Node from "../modules/Node"
import { stat } from "fs";



export interface Core {
    nodes: any[]
    solAncestorIds: number[]
}

type MyMap = Record<string, Node>

interface State {
    // core: Core
    // solNodeIds: number[]
    // id2Node: MyMap
}

interface Props {
    core: Core
}

export class TreeVis extends React.Component<Props, State>{

    constructor(props: Props) {

        console.log("constructor!!")

        super(props)
        // this.state = {core: props.core, solNodeIds: [], id2Node: {}}
        this.makeState = this.makeState.bind(this)

        this.state = {}
    }

    

    makeState(core: Core) {
        let state: any = {}
        state.solNodeIds = []
        state.id2Node = {}

        for (let i = 0; i < core.nodes.length; i++) {
            const newNode = core.nodes[i]
            const parentId = newNode.parentId

            // console.log(newNode)

            if (newNode.isSolution) {
                state.solNodeIds.push(newNode.id)
            }

            if (newNode.parentId === -1) {
                state.id2Node[newNode.id] = newNode
                continue
            }

            if (!state.id2Node[parentId].children) {
                state.id2Node[parentId].children = []
            }

            if (newNode.isLeftChild) {
                state.id2Node[parentId].children!.unshift(newNode)
            }

            else {
                state.id2Node[parentId].children!.push(newNode)
            }

            state.id2Node[newNode.id] = newNode
        }

        return state
    }

    

    render() {

        const map = this.makeState(this.props.core).id2Node
        const rootNode = d3.hierarchy(map[0])




        console.log("rendering...")
        console.log(map)
 

        return (
            <div>
                This is the tree
                <div>




                </div>


                <div id="treeWrapper" style={{ width: '50em', height: '20em' }}>
                    {map[0] && <Tree 
                    data={map[0]} 
                    orientation="vertical"
                    onClick={(nodeData, evt) => {
                        console.log("here!")
                    }}
                     />}
                </div>
            </div>
        )
    }
}
