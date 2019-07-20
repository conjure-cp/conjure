import * as React from "react"
import Node, { WhichTree } from "../../modules/Node"
import MergedTreeVis from "./MergedTreeVis"
import { HotKeys } from "react-hotkeys"
import { cloneDeep, last, min, max, isEqual } from "lodash"
import * as MovementHelper from "../../modules/MovementHelper"
import * as d3 from "d3"
import { mergeMaps, loadDiffs, getAncList } from "../../modules/ForestHelper"
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
        console.log("goLeft <-")
        // console.log(this.state.selectedTreeId)

        // --------------------------------------------------------------

        const currentSelected = this.state.selected

        let leftDiffIds = this.props.diffLocations.map(x => x[0])

        if (
          leftDiffIds.includes(currentSelected) &&
          this.state.selectedTreeId !== WhichTree.Right
        ) {
          console.log("leftDiffIds")

          let mergedMap = mergeMaps(
            this.state.leftMap!,
            this.state.rightMap!,
            this.props.diffLocations
          )

          let currentNode = mergedMap[currentSelected]
          // let nextIndex = Math.floor(currentNode.children!.length / 2)
          let nextIndex = 0
          let nextNode = currentNode.children![nextIndex]

          this.setState({
            selected: nextNode.id,
            selectedTreeId: nextNode.treeId
          })

          // console.log(nextNode)

          return
        }

        let isRightTree = this.state.selectedTreeId === WhichTree.Right

        let path = this.props.leftPath
        let map = this.state.leftMap
        let treeId = WhichTree.Left

        if (isRightTree) {
          // if (this.props.diffLocations.selected === origState.selected) {
          path = this.props.rightPath
          map = this.state.rightMap
          treeId = WhichTree.Right
        }

        let res = await MovementHelper.goLeftBoyo(
          currentSelected,
          map!,
          false,
          false,
          path,
          1,
          this.props.nimServerPort,
          treeId
        )

        let leftMap = this.state.leftMap!
        let rightMap = this.state.rightMap!

        if (isRightTree) {
          rightMap = res.id2Node
        } else {
          leftMap = res.id2Node
        }

        console.log("res.selected", res.selected)

        let mergedMap = mergeMaps(leftMap, rightMap, this.props.diffLocations)

        let revision = this.reviseGoLeft(
          mergedMap,
          leftMap,
          rightMap,
          res.selected,
          treeId,
          isRightTree,
          currentSelected
        )

        if (isRightTree) {
          this.setState({
            selected: revision.selected,
            selectedTreeId: revision.treeId,
            mergedMap: mergedMap
          })
        } else {
          this.setState({
            // leftMap: res.id2Node,
            selected: revision.selected,
            selectedTreeId: revision.treeId,
            mergedMap: mergedMap
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

  reviseGoLeft = (
    mergedMap: MyMap,
    leftMap: MyMap,
    rightMap: MyMap,
    selected: number,
    treeId: WhichTree,
    isRightTree: boolean,
    currentSelected: number
  ): { selected: number; treeId: WhichTree } => {
    // console.log("======================")
    // console.log(selected)
    // console.log(mergedMap[selected])
    // console.log(treeId)

    if (leftMap[selected] && !isRightTree) {
      console.log("here0")
      return { selected, treeId }
    }

    // If there is also a node with the same ID in the both section, then choose the one from the right tree

    if (
      leftMap[selected] &&
      leftMap[selected].treeId === WhichTree.Both &&
      isRightTree
    ) {
      console.log("here1")
      return { selected, treeId: WhichTree.Right }
    }

    let rightDiffIds = this.props.diffLocations.map(x => x[1])

    // If it aint in the left or both but is in the right then go right
    if (
      !leftMap[selected] &&
      rightMap[selected] &&
      isRightTree &&
      !rightDiffIds.includes(selected + 1)
    ) {
      console.log("here2")
      return { selected, treeId: WhichTree.Right }
    }

    if (
      leftMap[selected] &&
      leftMap[selected].treeId === WhichTree.Both &&
      isRightTree
    ) {
      console.log("here3")
      console.log(treeId)
      console.log("---")
      return { selected, treeId }
    }

    console.log("Revising")

    let ancestorIds = getAncList(mergedMap[0], currentSelected, treeId).map(
      y => y.data.id
    )

    let aboveDiffPoint = this.props.diffLocations.find(x =>
      ancestorIds.includes(x[0])
    )
    if (!aboveDiffPoint) {
      return { selected, treeId }
    }
    // console.log(ancestorIds)
    // console.log(aboveDiffPoint)
    // console.log(aboveDiffPoint[0] - 1)
    selected = mergedMap[aboveDiffPoint[0] - 1].children![1].id
    treeId = WhichTree.Both

    // console.log(selected)
    // console.log("!!!!!!!!")
    return { selected, treeId }
    // }
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

    // console.log(this.state.selected)

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
