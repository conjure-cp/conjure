import * as React from "react"
import Node, { WhichTree } from "../../modules/Node"
import MergedTreeVis from "./MergedTreeVis"
import { HotKeys } from "react-hotkeys"
import { cloneDeep, last, min, max, isEqual } from "lodash"
import * as MovementHelper from "../../modules/MovementHelper"
import * as d3 from "d3"
import { mergeMaps, loadDiffs } from "../../modules/ForestHelper"
import { FromServerNode, Core } from "./TreeContainer"
import { isTSImportEqualsDeclaration } from "@babel/types"

export type MyMap = Record<number, Node>

interface Props {
  leftCore: Core
  rightCore: Core
  leftSolAncestorIds: number[]
  rightSolAncestorIds: number[]
  diffLocations: number[][]
  rightPath: string
  leftPath: string
  hash: string
  nimServerPort: number
  loadDepth: number
}

export interface State {
  leftMap: MyMap | undefined
  rightMap: MyMap | undefined
  solveable: boolean
  selected: number
  selectedTreeId: WhichTree
  linScale: any
  minsize: number
  totalNodeCount: number
  failedBranchCount: number
}

const origState = {
  solveable: true,
  minsize: 5,
  selected: 0,
  selectedTreeId: WhichTree.Both,
  totalNodeCount: -1,
  failedBranchCount: -1,
  linScale: (_v: number) => 10,
  leftMap: undefined,
  rightMap: undefined
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

    if (isEqual(props.diffLocations, [[0, 0]])) {
      origState.selectedTreeId = WhichTree.Left
    }

    this.state = {
      ...origState
    }
    // this.state = TreeHelper.makeState(props.core, props.selected)

    this.handlers = {
      goLeft: async () => {
        console.log("here")
        console.log(this.state.selectedTreeId)

        let leftDiffIds = this.props.diffLocations.map(x => x[0])
        let rightDiffIds = this.props.diffLocations.map(x => x[1])

        let potentialLeftNode = null
        let potentialRightNode = null

        if (
          leftDiffIds.includes(this.state.selected) ||
          rightDiffIds.includes(this.state.selected)
        ) {
        }

        let onTheRightTree = this.state.selectedTreeId === WhichTree.Right

        let path = this.props.leftPath
        let map = this.state.leftMap
        let treeId = WhichTree.Left

        if (onTheRightTree) {
          // if (this.props.diffLocations.selected === origState.selected) {
          path = this.props.rightPath
          map = this.state.rightMap
          treeId = WhichTree.Right
        }

        let res = await MovementHelper.goLeftBoyo(
          this.state.selected,
          map!,
          false,
          false,
          path,
          1,
          this.props.nimServerPort
        )
        if (onTheRightTree) {
          this.setState({
            rightMap: res.id2Node,
            selected: res.selected,
            selectedTreeId: treeId
          })
        } else {
          this.setState({
            leftMap: res.id2Node,
            selected: res.selected,
            selectedTreeId: treeId
          })
        }
      },
      //   goUp: () => MovementHelper.goUp(this),
      //   goRight: () => MovementHelper.goRight(this),
      goToRoot: () => {
        console.log("GOT OT ROOT")
        this.setState({ selected: 0 })
      }
      //   goPrev: () => MovementHelper.goToPreviousHandler(this),
      //   collapse: this.collapse,
      //   expand: this.expand,
      //   showCore: this.showCore
    }
  }

  nodeClickHandler = (d: Node) => {
    this.setState({ selected: d.id, selectedTreeId: d.treeID })
  }

  loadAllDiffsIntoMaps = async () => {
    let maps = await loadDiffs(
      [this.props.leftPath, this.props.rightPath],
      [this.props.leftCore, this.props.rightCore],
      this.props.diffLocations,
      this.props.nimServerPort
    )

    this.setState({ leftMap: maps[0], rightMap: maps[1] })
  }

  componentDidMount = () => {
    this.loadAllDiffsIntoMaps()
  }

  componentDidUpdate = async (prevProps: Props, prevState: State) => {
    if (!isEqual(prevProps, this.props)) {
      await this.loadAllDiffsIntoMaps()
    }
  }

  render = () => {
    // TODO move this it shouldnt be here

    // console.log(this.state.selected)

    console.log(this.state.selectedTreeId)
    return (
      <HotKeys keyMap={this.map} handlers={this.handlers}>
        {/* <Wrapper> */}
        {/* <SplitPane split="horizontal" defaultSize={700} maxSize={900}> */}
        {this.state.leftMap && this.state.rightMap && (
          <MergedTreeVis
            hash={this.props.hash}
            leftDiffIds={this.props.diffLocations.map(x => x[0])}
            rightDiffIds={this.props.diffLocations.map(x => x[1])}
            identifier={"MergedTree"}
            rootNode={
              mergeMaps(
                this.state.leftMap,
                this.state.rightMap,
                this.props.diffLocations
              )[0]
            }
            selected={this.state.selected}
            selectedTreeId={this.state.selectedTreeId}
            leftSolAncestorIds={this.props.leftSolAncestorIds}
            rightSolAncestorIds={this.props.rightSolAncestorIds}
            solveable={this.state.solveable}
            linScale={this.state.linScale}
            minsize={this.state.minsize}
            nodeClickHandler={this.nodeClickHandler}
            storeNodePrevPos={list => {
              // this.setState((prevState: State) => {
              //   let newMap = cloneDeep(prevState.id2Node)
              //   list.forEach(d => {
              //     newMap[d.data.id].x0 = d.x
              //     newMap[d.data.id].y0 = d.y
              //   })
              //   console.log("CALLED")
              //   // console.log(newMap[28])
              //   return { id2Node: newMap }
              // })
            }}
            duration={1000}
            width={1200}
            height={500}
            showLabels={true}
            diffParentId={-1}
          />
        )}
      </HotKeys>
    )
  }
}
