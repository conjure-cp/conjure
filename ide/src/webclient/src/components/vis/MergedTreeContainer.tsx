import * as React from "react"
import Node, { WhichTree } from "../../modules/Node"
import MergedTreeVis from "./MergedTreeVis"
import { HotKeys } from "react-hotkeys"
import { cloneDeep, last, min, max } from "lodash"
import * as MovementHelper from "../../modules/MovementHelper"
import * as d3 from "d3"

interface FromServerNode {
  id: number
  parentId: number
  label: string
  prettyLabel: string
  childCount: number
  isSolution: boolean
  isLeftChild: boolean
  descCount: number
}

export type MyMap = Record<number, Node>

export interface Core {
  nodes: FromServerNode[]
  solAncestorIds: number[]
}

interface Props {
  map: MyMap
  //   selected: number
  //   identifier: string
  //   core: Core
  //   info: string
  leftSolAncestorIds: number[]
  rightSolAncestorIds: number[]
  leftDiffIds: number[]
  rightDiffIds: number[]
  rightPath: string
  leftPath: string
  hash: string
  nimServerPort: number
  //   playing: boolean
  //   reverse: boolean
  //   showLabels: boolean
  loadDepth: number
  //   duration: number
  //   interval: number
  //   finishedPlayingHandler: () => void
  //   collapseAsExploring: boolean
  //   diffParentId: number
}

export interface State {
  id2Node: MyMap
  solveable: boolean
  selected: number
  linScale: any
  minsize: number
  totalNodeCount: number
  failedBranchCount: number
}

export class MergedTreeContainer extends React.Component<Props, State> {
  // static whyDidYouRender = true;

  map = {
    goLeft: ["left", "s", "a", "down"],
    goRight: ["right", "d"],
    goUp: ["up", "w"],
    collapse: "c",
    expand: "e",
    pPressed: "p",
    goToRoot: "r",
    goPrev: "shift",
    showCore: "f"
  }

  handlers: any

  constructor(props: Props) {
    super(props)

    this.state = {
      id2Node: props.map,
      solveable: true,
      minsize: 5,
      selected: 0,
      totalNodeCount: -1,
      failedBranchCount: -1,
      linScale: (_v: number) => 10
      //   linScale: d3
      //     .scaleLinear()
      //     .domain([0, props.map[0].descCount])
      //     .range([5, 20])
    }
    // this.state = TreeHelper.makeState(props.core, props.selected)

    this.handlers = {
        goLeft: () => {
            
     

      },
      //   goUp: () => MovementHelper.goUp(this),
      //   goRight: () => MovementHelper.goRight(this),
      goToRoot: () => this.setState({ selected: 0 })
      //   goPrev: () => MovementHelper.goToPreviousHandler(this),
      //   collapse: this.collapse,
      //   expand: this.expand,
      //   showCore: this.showCore
    }
  }

  nodeClickHandler = (d: Node) => {
    this.setState({ selected: d.id })
  }

  collapse = () => {
    this.setState((prevState: State) => {
      const toCollapse = prevState.selected

      console.log("Collapsing ", toCollapse)

      let newMap = cloneDeep(prevState.id2Node)
      Node.collapseNode(newMap[toCollapse])
      // Node.hideChildren(newMap[toCollapse])
      return { id2Node: newMap }
    })
  }

  expand = () => {
    this.setState((prevState: State) => {
      const toExpand = prevState.selected
      let newMap = cloneDeep(prevState.id2Node)
      Node.expandNode(newMap[toExpand])
      // Node.showChildren(newMap[toExpand])
      return { id2Node: newMap }
    })
  }

  componentDidUpdate = async (prevProps: Props, prevState: State) => {
    // if (this.state.goingLeft !== prevState.goingLeft) {
    //   const current = this.state.id2Node[this.state.selected]
    //   const path =
    //     current.treeID === WhichTree.Right
    //       ? this.props.rightPath
    //       : this.props.leftPath

    //   console.log("here")

    //   this.setState(
    //     await MovementHelper.goLeftBoyo(
    //       this.state.selected,
    //       this.state.id2Node,
    //       false,
    //       false,
    //       path,
    //       this.props.loadDepth,
    //       this.props.nimServerPort
    //     )
    //   )
    // }
  }

  render = () => {
    // TODO move this it shouldnt be here

    // console.log(this.state.id2Node[0])
    return (
      <HotKeys keyMap={this.map} handlers={this.handlers}>

        {/* <Wrapper> */}
        {/* <SplitPane split="horizontal" defaultSize={700} maxSize={900}> */}
        <MergedTreeVis
          hash={this.props.hash}
          leftDiffIds={this.props.leftDiffIds}
          rightDiffIds={this.props.rightDiffIds}
          identifier={"MergedTree"}
          rootNode={this.state.id2Node[0]}
          selected={this.state.selected}
          leftSolAncestorIds={this.props.leftSolAncestorIds}
          rightSolAncestorIds={this.props.rightSolAncestorIds}
          solveable={this.state.solveable}
          linScale={this.state.linScale}
          minsize={this.state.minsize}
          nodeClickHandler={this.nodeClickHandler}
          storeNodePrevPos={list => {
            this.setState((prevState: State) => {
              let newMap = cloneDeep(prevState.id2Node)
              list.forEach(d => {
                newMap[d.data.id].x0 = d.x
                newMap[d.data.id].y0 = d.y
              })
              console.log("CALLED")
              // console.log(newMap[28])
              return { id2Node: newMap }
            })
          }}
          duration={1000}
          width={1200}
          height={500}
          showLabels={true}
          diffParentId={-1}
        />
      </HotKeys>
    )
  }
}
