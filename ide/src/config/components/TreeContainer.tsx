import * as React from "react"
import * as ReactDOM from "react-dom"
import Node from "../modules/Node"
import TreeVis from "./TreeVis"
import StatsBar from "./StatsBar"
import { HotKeys, GlobalHotKeys } from "react-hotkeys"
import { cloneDeep, last, min, max } from "lodash"
import * as d3 from "d3"
import { Domains } from "./Domains"
import SplitPane from "react-split-pane"
import { Wrapper } from "./Constants"
import { Check } from "./Check"
import {
  getNextSolId,
  getPrevSolId,
  showAllAncestors,
  getNextFailedId,
  getPrevFailedId
} from "../modules/Helper"

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
  path: string
  nimServerPort: number
  playing: boolean
  reverse: boolean
  loadDepth: number
  duration: number
  finishedPlayingHandler: () => void
}

export interface State {
  id2Node: MyMap
  solveable: boolean
  selected: number
  linScale: any
  minsize: number
  shouldGetKids: boolean
  solNodeIds: number[]
  totalNodeCount: number
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
    solNodeIds: solNodeIds,
    totalNodeCount: last(core.solAncestorIds)! + 1
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
        this.setState((prevState: State) => {
          const newMap = showAllAncestors(
            prevState,
            this.props.core.solAncestorIds[currentIndex + 1]
          )
          return {
            selected: this.props.core.solAncestorIds[currentIndex + 1],
            id2Node: newMap
          }
        })
      }
      return
    }

    const nextId = min(
      this.props.core.solAncestorIds.filter(num => {
        return this.state.selected < num
      })
    )

    if (!nextId) {
      return
    }

    this.setState((prevState: State) => {
      const newMap = showAllAncestors(prevState, nextId)
      return {
        selected: nextId,
        id2Node: newMap
      }
    })
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
        this.setState((prevState: State) => {
          const newMap = showAllAncestors(
            prevState,
            this.props.core.solAncestorIds[currentIndex - 1]
          )
          return {
            selected: this.props.core.solAncestorIds[currentIndex - 1],
            id2Node: newMap
          }
        })
      }
      return
    }

    const nextId = max(
      this.props.core.solAncestorIds.filter(num => {
        return this.state.selected > num
      })
    )

    if (!nextId) {
      return
    }

    this.setState((prevState: State) => {
      const newMap = showAllAncestors(prevState, nextId)
      return {
        selected: nextId,
        id2Node: newMap
      }
    })
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

    const payload = {
      path: this.props.path,
      nodeId: nextId
    }

    fetch(`http://localhost:${this.props.nimServerPort}/loadAncestors`, {
      method: "post",
      headers: {
        Accept: "application/json, text/plain, */*",
        "Content-Type": "text/plain"
      },
      body: JSON.stringify(payload)
    })
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

    const payload = {
      path: this.props.path,
      nodeId: this.state.selected,
      depth: this.props.loadDepth
    }

    fetch(`http://localhost:${this.props.nimServerPort}/loadNodes`, {
      method: "post",
      headers: {
        Accept: "application/json, text/plain, */*",
        "Content-Type": "text/plain"
      },
      body: JSON.stringify(payload)
    })
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
    if (!this.state.solveable) {
      this.goToPreviousHandler()
      return
    }

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

      if (nextId === -1) {
        return
      }

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

      if (solId === -1) {
        return null
      }

      const newMap = showAllAncestors(prevState, solId)
      return { selected: solId, id2Node: newMap }
    })
  }

  nextSol = () => {
    this.setState((prevState: State) => {
      const solId = getNextSolId(prevState)

      if (solId === -1) {
        return null
      }

      const newMap = showAllAncestors(prevState, solId)
      return { selected: solId, id2Node: newMap }
    })
  }

  goToRoot = () => {
    this.setState({ selected: 0 })
  }

  play = async () => {
    while (this.props.playing) {
      if (
        (this.state.selected === last(this.props.core.solAncestorIds)! &&
          !this.props.reverse) ||
        (this.state.selected === 0 && this.props.reverse)
      ) {
        break
      }
      if (this.props.reverse) {
        this.goToPreviousHandler()
      } else {
        this.goLeft()
      }
      await this.sleep(this.props.duration)
    }
    this.props.finishedPlayingHandler()
  }

  sleep = (ms: number) => {
    return new Promise(resolve => setTimeout(resolve, ms))
  }

  componentDidUpdate = (prevProps: Props) => {
    // Typical usage (don't forget to compare props):
    if (this.props.core.id !== prevProps.core.id) {
      this.setState(makeState(this.props.core))
    }

    if (this.props.playing !== prevProps.playing) {
      this.play()
    }
  }

  render = () => {
    let failedBranchCount =
      this.state.totalNodeCount -
      (this.state.solveable ? this.props.core.solAncestorIds.length : 0)

    return (
      <div className="treeContainer">
        <HotKeys keyMap={map} handlers={this.handlers}>
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
            totalNodes={this.state.totalNodeCount}
            failedBranchCount={failedBranchCount}
            linScale={this.state.linScale}
          />

          <Wrapper>
            <SplitPane split="horizontal" defaultSize={600}>
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
                duration={this.props.duration}
                width={1200}
                height={500}
              />

              <Domains
                id={this.props.core.id}
                selected={this.state.selected}
                path={this.props.path}
                nimServerPort={this.props.nimServerPort}
              />
            </SplitPane>
          </Wrapper>
        </HotKeys>
      </div>
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
