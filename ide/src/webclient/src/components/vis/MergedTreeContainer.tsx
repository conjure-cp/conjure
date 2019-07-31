import * as React from "react"
import Node, { WhichTree } from "../../modules/Node"
import MergedTreeVis from "./MergedTreeVis"
import { HotKeys } from "react-hotkeys"
import { cloneDeep, last, min, max, isEqual } from "lodash"
import * as MovementHelper from "../../modules/MovementHelper"
import * as d3 from "d3"
import {
  mergeMaps,
  loadAllDiffs,
  getAncList,
  loadDiff,
  assignTreeIds,
  collapseUnwantedDiffs
} from "../../modules/ForestHelper"
import { FromServerNode, Core } from "./TreeContainer"
import {
  isTSImportEqualsDeclaration,
  exportDefaultSpecifier
} from "@babel/types"
import {
  goLeftAtDiffingPoint,
  reviseGoLeft,
  shouldBeRightTree,
  goLeftMerged,
  goUpMerged,
  goDownMerged,
  goRightMerged,
  collapseMerged,
  expandMerged
} from "../../modules/MergedTreeHelper"
import { tree } from "d3"
import { makeState } from "../../modules/TreeHelper"
import { DiffPoint } from "../Forest"

export type MyMap = Record<number, Node>

interface Props {
  leftCore: Core
  rightCore: Core
  leftSolAncestorIds: number[]
  rightSolAncestorIds: number[]
  diffPoints: DiffPoint[]
  currentDiff?: DiffPoint
  rightPath: string
  leftPath: string
  hash: string
  nimServerPort: number
  loadDepth: number
  duration: number
  interval: number
  showLabels: boolean
}

export interface State {
  leftMap: MyMap
  rightMap: MyMap
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
  linScale: (_v: number) => 10
}

export class MergedTreeContainer extends React.Component<Props, State> {
  // static whyDidYouRender = true;

  map = {
    goLeft: ["left", "a"],
    goRight: ["right", "d"],
    goUp: ["up", "w"],
    goDown: ["down", "s"],
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

    if (isEqual(props.diffPoints, [[0, 0]])) {
      origState.selectedTreeId = WhichTree.Left
    }

    let leftMap = makeState(props.leftCore, 0).id2Node
    let rightMap = makeState(props.rightCore, 0).id2Node

    collapseUnwantedDiffs(leftMap, rightMap, props.diffPoints)

    assignTreeIds(leftMap, rightMap, props.diffPoints)

    this.state = {
      ...origState,
      leftMap: leftMap,
      rightMap: rightMap,
      // mergedMap: mergeMaps(leftMap, rightMap, props.diffLocations, props.augmentedIds),
      solveable:
        props.leftCore.nodes
          .concat(props.rightCore.nodes)
          .find(x => x.isSolution) !== undefined
    }

    this.handlers = {
      goLeft: async () => {
        this.setState(
          await goLeftMerged(
            this.state.selected,
            this.state.selectedTreeId,
            this.props.leftPath,
            this.props.rightPath,
            this.state.leftMap!,
            this.state.rightMap!,
            this.props.diffPoints,
            this.props.nimServerPort
          )
        )
      },
      goUp: () => {
        this.setState(
          goUpMerged(
            this.state.leftMap!,
            this.state.rightMap!,
            this.state.selected,
            this.state.selectedTreeId,
            this.props.diffPoints
          )
        )
      },

      goDown: async () => {
        console.log("calling go down")
        this.setState(
          await goDownMerged(
            this.state.leftMap,
            this.state.rightMap,
            this.state.selected,
            this.state.selectedTreeId,
            this.props.diffPoints,
            this.props.leftPath,
            this.props.rightPath,
            this.props.nimServerPort
          )
        )
      },
      goRight: async () => {
        this.setState(
          await goRightMerged(
            this.state.leftMap,
            this.state.rightMap,
            this.state.selected,
            this.state.selectedTreeId,
            this.props.diffPoints
          )
        )
      },
      goToRoot: () => {
        this.setState({ selected: 0, selectedTreeId: WhichTree.Both })
      },
      collapse: () => {
        this.setState(
          collapseMerged(
            this.state.leftMap,
            this.state.rightMap,
            this.state.selected,
            this.state.selectedTreeId,
            this.props.diffPoints
          )
        )
      },
      expand: () => {
        this.setState(
          expandMerged(
            this.state.leftMap,
            this.state.rightMap,
            this.state.selected,
            this.state.selectedTreeId,
            this.props.diffPoints
          )
        )
      }
    }
  }

  nodeClickHandler = (d: Node) => {
    this.setState({ selected: d.id, selectedTreeId: d.treeId })
  }

  loadDiffsFromAbove = async () => {
    let selected = this.state.selected

    let maps = [this.state.leftMap, this.state.rightMap]

    if (this.props.currentDiff) {
      // console.log(this.props.currentDiff)

      maps = await loadDiff(
        [this.props.leftPath, this.props.rightPath],
        maps,
        this.props.currentDiff,
        this.props.nimServerPort
      )
      selected = this.props.currentDiff.leftTreeId
    }

    this.setState({
      leftMap: maps[0],
      rightMap: maps[1],
      selected: selected
    })
  }

  componentDidMount = () => {
    this.loadDiffsFromAbove()
  }

  componentDidUpdate = async (prevProps: Props, prevState: State) => {
    if (!isEqual(prevProps, this.props)) {
      await this.loadDiffsFromAbove()
    }
  }

  render = () => {
    if (this.state.rightMap) {
      // console.log("---------------------")
      // console.log(this.state.leftMap)
      // console.log(this.state.rightMap)
    }

    return (
      <HotKeys keyMap={this.map} handlers={this.handlers}>
        {/* <Wrapper> */}
        {/* <SplitPane split="horizontal" defaultSize={700} maxSize={900}> */}
        {this.state.leftMap && this.state.rightMap && (
          <MergedTreeVis
            hash={this.props.hash}
            identifier={"MergedTree"}
            rootNode={
              // this.state.mergedMap[0]
              mergeMaps(
                this.state.leftMap,
                this.state.rightMap,
                this.props.diffPoints
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
              this.setState((prevState: State) => {
                let leftMap = cloneDeep(prevState.leftMap)
                let rightMap = cloneDeep(prevState.rightMap)

                list.forEach(d => {
                  if (d.data.treeId === WhichTree.Right) {
                    rightMap![d.data.id].x0 = d.x
                    rightMap![d.data.id].y0 = d.y
                  } else {
                    leftMap![d.data.id].x0 = d.x
                    leftMap![d.data.id].y0 = d.y
                  }
                })
                console.log("CALLED")
                // console.log(newMap[28])
                return { leftMap, rightMap }
              })
            }}
            duration={this.props.duration}
            width={1200}
            height={500}
            showLabels={this.props.showLabels}
            diffParentId={-1}
          />
        )}
      </HotKeys>
    )
  }
}
