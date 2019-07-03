import * as React from "react"
import * as ReactDOM from "react-dom"
import Node from "../modules/Node"
import TreeVis from "./TreeVis"
import StatsBar from "./StatsBar"
import Play from "./Play"
import { HotKeys, GlobalHotKeys } from "react-hotkeys"
import { cloneDeep, last, min, max } from "lodash"
import * as d3 from "d3"
import StageHeader from "./StageHeader"
import { Domains } from "./Domains"
import { thresholdScott, HierarchyCircularNode, HierarchyPointNode } from "d3"
import {
  getNextSolId,
  getPrevSolId,
  showAllAncestors,
  getNextFailedId,
  getPrevFailedId
} from "../modules/Helper"
import SplitPane, * as Blah from "react-split-pane"
import { MySlider } from "./Slider"
import { Check } from "./Check"
// import * as Handle from "./Slider",
// import SplitterLayout from "react-splitter-layout"
// import "react-splitter-layout/lib/index.css"

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
  id: string
}

interface Props {
  identifier: string
  core: Core
  info: string
}

export interface State {
  id2Node: MyMap
  solveable: boolean
  selected: number
  linScale: any
  minsize: number
  shouldGetKids: boolean
  playing: boolean
  solNodeIds: number[]
  loadDepth: number
  reverse: boolean
}

const makeState = (core: Core): State => {
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
    playing: false,
    solNodeIds: solNodeIds,
    loadDepth: 1,
    reverse: false
  }

  return state
}

export class TreeContainer extends React.Component<Props, State> {
  // static whyDidYouRender = true;

  handlers: any

  constructor(props: Props) {
    super(props)
    this.state = makeState(this.props.core)

    this.handlers = {
      goLeft: this.goLeft,
      goUp: this.goUp,
      goRight: this.goRight,
      collapse: this.collapse,
      expand: this.expand,
      pPressed: this.pPressed,
      goToRoot: this.goToRoot,
      goPrev: this.goToPreviousHandler
    }
  }

  nodeClickHandler = (d: Node) => {
    this.setState({ selected: d.id })
  }

  insertNodes = (nodes: Node[], nextId: number) => {
    this.setState((prevState: State) => {
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

  nextSolBranch = () => {
    if (!this.state.solveable) {
      return
    }

    if (this.props.core.solAncestorIds.includes(this.state.selected)) {
      const currentIndex = this.props.core.solAncestorIds.indexOf(
        this.state.selected
      )
      if (currentIndex + 1 < this.props.core.solAncestorIds.length) {
        this.setState({
          selected: this.props.core.solAncestorIds[currentIndex + 1]
        })
      }
      return
    }

    const nextId = min(
      this.props.core.solAncestorIds.filter(num => {
        return this.state.selected < num
      })
    )!

    this.setState({ selected: nextId })
  }

  prevSolBranch = () => {
    if (!this.state.solveable) {
      return
    }

    if (this.props.core.solAncestorIds.includes(this.state.selected)) {
      const currentIndex = this.props.core.solAncestorIds.indexOf(
        this.state.selected
      )
      if (currentIndex - 1 >= 0) {
        this.setState({
          selected: this.props.core.solAncestorIds[currentIndex - 1]
        })
      }
      return
    }

    const nextId = max(
      this.props.core.solAncestorIds.filter(num => {
        return this.state.selected > num
      })
    )!

    this.setState({ selected: nextId })
  }

  goToPreviousHandler = () => {
    this.goPrev()
  }

  goPrev = (start?: number) => {
    let current = start ? start : this.state.selected

    console.log(current)

    if (current === 0) {
      return
    }

    const nextId = current - 1

    if (nextId in this.state.id2Node) {
      this.setState((prevState: State) => {
        const newMap = showAllAncestors(prevState, nextId)
        return { selected: nextId, id2Node: newMap }
      })
      return
    }

    fetch(`http://localhost:5000/loadAncestors/${nextId}`)
      .then(data => data.json())
      .then(nodes => this.insertNodes(nodes, nextId))
  }

  goLeft = () => {
    const nextId = this.state.selected + 1

    if (nextId in this.state.id2Node) {
      this.setState((prevState: State) => {
        const newMap = showAllAncestors(prevState, this.state.selected)
        return { selected: nextId, id2Node: newMap }
      })
      return
    }

    fetch(
      `http://localhost:5000/loadNodes/${this.state.selected}/${this.state.loadDepth}`
    )
      .then(data => data.json())
      .then(nodes => this.insertNodes(nodes, this.state.selected))
  }

  goRight = () => {
    this.setState((prev: State) => {
      const current = prev.id2Node[prev.selected]
      if (!current.children) {
        return null
      }
      if (current.children.length < 2) {
        return null
      }
      return { selected: current.children[1].id }
    })
  }

  goUp = () => {
    this.setState((prev: State) => {
      const current = prev.id2Node[prev.selected]
      if (current.parentId === -1) {
        return null
      }
      return { selected: current.parentId }
    })
  }

  collapse = () => {
    this.setState((prevState: State) => {
      let newMap = cloneDeep(prevState.id2Node)
      Node.collapseNode(newMap[prevState.selected])
      return { id2Node: newMap }
    })
  }

  expand = () => {
    this.setState((prevState: State) => {
      let newMap = cloneDeep(prevState.id2Node)
      Node.expandNode(newMap[prevState.selected])
      return { id2Node: newMap }
    })

    // console.log("expanded!");
  }

  nextFailed = () => {
    if (!this.state.solveable) {
      this.goLeft()
      return
    }

    if (this.props.core.solAncestorIds.includes(this.state.selected)) {
      let nextId = getNextFailedId(
        this.state.selected,
        this.props.core.solAncestorIds
      )
      if (nextId !== -1) {
        this.setState((prevState: State) => {
          const newMap = showAllAncestors(prevState, nextId)
          return { selected: nextId, id2Node: newMap }
        })
      }
      return
    }

    if (this.props.core.solAncestorIds.includes(this.state.selected + 1)) {
      let nextId = getNextFailedId(
        this.state.selected + 1,
        this.props.core.solAncestorIds
      )
      if (nextId !== -1) {
        this.setState((prevState: State) => {
          const newMap = showAllAncestors(prevState, nextId)
          return { selected: nextId, id2Node: newMap }
        })
      }
    } else {
      this.goLeft()
    }
  }

  prevFailed = () => {
    if (this.props.core.solAncestorIds.includes(this.state.selected)) {
      let nextId = getPrevFailedId(
        this.state.selected,
        this.props.core.solAncestorIds
      )

      if (nextId in this.state.id2Node) {
        this.setState((prevState: State) => {
          const newMap = showAllAncestors(prevState, nextId)
          return { selected: nextId, id2Node: newMap }
        })
      } else {
        this.goPrev(nextId + 1)
      }
      return
    }

    if (this.props.core.solAncestorIds.includes(this.state.selected - 1)) {
      let nextId = getPrevFailedId(
        this.state.selected - 1,
        this.props.core.solAncestorIds
      )

      if (nextId in this.state.id2Node) {
        this.setState((prevState: State) => {
          const newMap = showAllAncestors(prevState, nextId)
          return { selected: nextId, id2Node: newMap }
        })
      } else {
        this.goPrev(nextId + 1)
      }
    } else {
      this.goPrev()
    }
  }

  prevSol = () => {
    this.setState((prevState: State) => {
      const solId = getPrevSolId(prevState)
      const newMap = showAllAncestors(prevState, solId)
      return { selected: solId, id2Node: newMap }
    })
  }

  nextSol = () => {
    this.setState((prevState: State) => {
      const solId = getNextSolId(prevState)
      const newMap = showAllAncestors(prevState, solId)
      return { selected: solId, id2Node: newMap }
    })
  }

  pPressed = () => {
    this.setState((prevState: State) => {
      return {
        playing: !prevState.playing
      }
    })
  }

  goToRoot = () => {
    this.setState({ selected: 0 })
  }

  play = async () => {
    const interval = 400
    while (this.state.playing) {
      if (
        (this.state.selected === last(this.props.core.solAncestorIds)! &&
          !this.state.reverse) ||
        (this.state.selected === 0 && this.state.reverse)
      ) {
        break
      }
      if (this.state.reverse) {
        this.goToPreviousHandler()
      } else {
        this.goLeft()
      }
      await this.sleep(interval)
    }
    this.setState({ playing: false })
  }

  sleep = (ms: number) => {
    return new Promise(resolve => setTimeout(resolve, ms))
  }

  componentDidUpdate = (prevProps: Props, prevState: State) => {
    // Typical usage (don't forget to compare props):
    if (this.props.core.id !== prevProps.core.id) {
      this.setState(makeState(this.props.core))
    }

    if (this.state.playing !== prevState.playing) {
      this.play()
    }
  }

  render = () => {
    let failedBranchCount =
      this.state.id2Node[0].descCount -
      (this.state.solveable ? this.props.core.solAncestorIds.length : 0)

    return (
      <GlobalHotKeys keyMap={map} handlers={this.handlers}>
        <div className="treeContainer">
          <StatsBar
            info={this.props.info}
            nextSolBranchHandler={this.nextSolBranch}
            prevSolBranchHandler={this.prevSolBranch}
            nextNodeHandler={this.goLeft}
            prevNodeHandler={this.goToPreviousHandler}
            nextFailedHandler={this.nextFailed}
            prevFailedHandler={this.prevFailed}
            nextSolHandler={this.nextSol}
            prevSolHandler={this.prevSol}
            minsize={this.state.minsize}
            solNodeIds={this.state.solNodeIds}
            totalNodes={last(this.props.core.solAncestorIds)!}
            failedBranchCount={failedBranchCount}
            linScale={this.state.linScale}
          />

          <div className="sliderContainer row">
            <label className="col-1-2">Lazy loading depth:</label>
            <div className="slider col-1">
              <MySlider
                sliderChangeHandler={(value: number) => {
                  this.setState({ loadDepth: value })
                }}
              />
            </div>

            <div className="col">
              <Check
                title={"Reverse play order"}
                checked={this.state.reverse}
                onChange={() => {
                  this.setState((prevState: State) => {
                    return { reverse: !prevState.reverse }
                  })
                }}
              />
            </div>

            <div className="player col mb-3">
              <Play
                clickHandler={this.pPressed}
                playing={this.state.playing}
                x={0}
              />
            </div>
          </div>

          <Wrapper>
            <SplitPane split="horizontal" defaultSize={600}>
              {/* <Split> */}
              <TreeVis
                id={this.props.core.id}
                identifier={this.props.identifier}
                rootNode={this.state.id2Node[0]}
                selected={this.state.selected}
                solAncestorIds={this.props.core.solAncestorIds}
                solveable={this.state.solveable}
                linScale={this.state.linScale}
                minsize={this.state.minsize}
                nodeClickHandler={this.nodeClickHandler}
                duration={500}
                width={1200}
                height={500}
              />

              <Domains id={this.props.core.id} selected={this.state.selected} />
            </SplitPane>
          </Wrapper>
        </div>
      </GlobalHotKeys>
    )
  }
}

const map = {
  goLeft: ["left", "s", "a", "down"],
  goRight: ["right", "d"],
  goUp: ["up", "w"],
  collapse: "c",
  expand: "e",
  pPressed: "p",
  goToRoot: "r",
  goPrev: "shift"
}

const Wrapper = (props: any) => (
  <div
    style={{
      flex: 1,
      display: "flex",
      height: "100%",
      width: "100%",
      border: "1px solid red",
      position: "relative"
    }}
  >
    {props.children}
  </div>
)
