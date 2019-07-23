import * as React from "react"
import Node, { WhichTree } from "../../modules/Node"
import MergedTreeVis from "./MergedTreeVis"
import { HotKeys } from "react-hotkeys"
import { cloneDeep, last, min, max, isEqual } from "lodash"
import * as MovementHelper from "../../modules/MovementHelper"
import * as d3 from "d3"
import { mergeMaps, loadDiffs, getAncList } from "../../modules/ForestHelper"
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
  goDownMerged
} from "../../modules/MergedTreeHelper"
import { tree } from "d3"

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
  leftMap?: MyMap
  rightMap?: MyMap
  mergedMap?: MyMap
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

    if (isEqual(props.diffLocations, [[0, 0]])) {
      origState.selectedTreeId = WhichTree.Left
    }

    this.state = {
      ...origState
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
            this.props.diffLocations,
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
            this.props.diffLocations
          )
        )
      },

      goDown: async () => {
        this.setState(
          await goDownMerged(
            this.state.leftMap!,
            this.state.rightMap!,
            this.state.selected,
            this.state.selectedTreeId,
            this.props.diffLocations,
            this.props.leftPath,
            this.props.rightPath,
            this.props.nimServerPort
          )
        )
      },
      goToRoot: () => {
        console.log("GOT OT ROOT")
        this.setState({ selected: 0, selectedTreeId: WhichTree.Both })
      }
      //   goPrev: () => MovementHelper.goToPreviousHandler(this),
      //   collapse: this.collapse,
      //   expand: this.expand,
      //   showCore: this.showCore
    }
  }

  nodeClickHandler = (d: Node) => {
    this.setState({ selected: d.id, selectedTreeId: d.treeId })
  }

  loadAllDiffsIntoMaps = async () => {
    let maps = await loadDiffs(
      [this.props.leftPath, this.props.rightPath],
      [this.props.leftCore, this.props.rightCore],
      this.props.diffLocations,
      this.props.nimServerPort
    )

    this.setState({
      leftMap: maps[0],
      rightMap: maps[1],
      mergedMap: mergeMaps(maps[0], maps[1], this.props.diffLocations)
    })
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

    if (this.state.rightMap) {
      console.log(this.props.diffLocations)
      console.log(this.state.rightMap[9])
    }

    // console.log("selected", this.state.selected, this.state.selectedTreeId)

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
