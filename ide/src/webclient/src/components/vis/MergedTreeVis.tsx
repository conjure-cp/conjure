import * as React from "react"
import * as ReactDOM from "react-dom"
import * as d3 from "d3"
import Node, { WhichTree } from "../../modules/Node"
import {
  HierarchyPointLink,
  HierarchyPointNode,
  Selection,
  precisionFixed
} from "d3"
import { linkGenerator } from "../../modules/TreeHelper"
import { isEqual, reduce, cloneDeep, reduceRight } from "lodash"

type num2num = Record<number, { x: number; y: number }>

interface Props {
  diffParentId: number
  showLabels: boolean
  hash: string
  identifier: string
  width: number
  height: number
  rootNode: Node
  duration: number
  selected: number
  selectedTreeId: WhichTree
  leftSolAncestorIds: number[]
  rightSolAncestorIds: number[]
  leftDiffIds: number[]
  rightDiffIds: number[]
  solveable: boolean
  linScale: any
  minsize: number
  nodeClickHandler: (d: Node) => void
  storeNodePrevPos: (list: HierarchyPointNode<Node>[]) => void
}

interface State {
  oldPos: num2num
}

interface Link {
  source: HierarchyPointNode<Node>
  target: HierarchyPointNode<Node>
  highlight: WhichTree
}

export default class MergedTreeVis extends React.Component<Props, State> {
  // static whyDidYouRender = true
  zoom: any

  constructor(props: Props) {
    super(props)
    this.state = { oldPos: {} }

    this.zoom = d3
      .zoom<any, any>()
      .on("zoom", function() {
        d3.select(`#${props.identifier}thegroup`).attr(
          "transform",
          d3.event.transform
        )
      })
      .extent([[0, 0], [props.width, props.height]])
  }

  focusNode(node: HierarchyPointNode<Node>) {
    this.zoom.translateTo(
      d3
        .select(`#${this.props.identifier} svg`)
        .transition()
        .duration(this.props.duration),
      node.x,
      node.y
    )
  }

  hasHiddenChildren(d: HierarchyPointNode<Node>): boolean {
    return d.data.childCount > (d.data.children ? d.data.children.length : 0)
  }

  getSolAncestorIds(d: HierarchyPointNode<Node>): number[] {
    if (d.data.treeId === WhichTree.Left || d.data.treeId === WhichTree.Both) {
      return this.props.leftSolAncestorIds
    }
    return this.props.rightSolAncestorIds
  }

  hasOnlyExploredChildren(d: HierarchyPointNode<Node>): boolean {
    if (d.children || d.data.childCount === 0) {
      return false
    }

    let solAncestorIds = this.getSolAncestorIds(d)

    if (solAncestorIds.includes(d.data.id)) {
      return false
    }

    for (let i = 0; i < d.data.descCount; i++) {
      if (!(d.data.id + i + 1 in this.state.oldPos)) {
        return false
      }
    }

    return true
  }

  getDecCountMessage(d: HierarchyPointNode<Node>): string {
    if (this.hasHiddenChildren(d)) {
      return d.data.descCount + " nodes below"
    }
    return ""
  }

  maybeFocus(d: HierarchyPointNode<Node>): void {
    if (this.isSelected(d)) {
      this.focusNode(d)
    }
  }

  isSelected = (d: HierarchyPointNode<Node>) => {
    return d.data.id === this.props.selected
    // if (this.props.selectedTreeId === WhichTree.Right) {
    //   if (
    //     d.data.id === this.props.selected &&
    //     d.data.treeId === this.props.selectedTreeId
    //   ) {
    //     return true
    //   }
    // } else {
    //   if (d.data.id === this.props.selected && d.data.treeId !== WhichTree.Right) {
    //     return true
    //   }
    // }

    // return false
  }

  updateCircles(selector: any) {
    let circle = selector.select("circle")

    circle
      .transition()
      .duration(this.props.duration)
      .attr("r", (d: HierarchyPointNode<Node>) =>
        Node.getRadius(
          d,
          this.props.linScale,
          this.props.minsize,
          this.props.diffParentId
        )
      )

    circle.style(
      "transition",
      `stroke ${this.props.duration}ms, fill ${this.props.duration}ms`
    )

    circle.classed("different", (d: HierarchyPointNode<Node>) => {
      return d.data.id === this.props.diffParentId
    })

    circle.classed("selected", (d: HierarchyPointNode<Node>) =>
      this.isSelected(d)
    )

    circle.classed("hasOthers", (d: HierarchyPointNode<Node>) =>
      this.hasHiddenChildren(d)
    )

    circle.classed("red", (d: HierarchyPointNode<Node>) => {
      let solAncestorIds = this.getSolAncestorIds(d)

      return (
        (!solAncestorIds.includes(d.data.id) || !this.props.solveable) &&
        !this.hasOnlyExploredChildren(d)
      )
    })

    circle.classed("explored", (d: HierarchyPointNode<Node>) =>
      this.hasOnlyExploredChildren(d)
    )

    circle.classed(
      "solution",
      (d: HierarchyPointNode<Node>) => d.data.isSolution
    )
  }

  getPrevPos = (d: HierarchyPointNode<Node>): { x: number; y: number } => {
    return this.state.oldPos[d.data.id]
      ? this.state.oldPos[d.data.id]
      : { x: -1, y: -1 }
  }

  getParentNode = (
    d: HierarchyPointNode<Node>,
    nodeList: HierarchyPointNode<Node>[]
  ) => {
    return nodeList.filter(k => k.data.id === d.parent!.data.id)[0]
  }

  getNode = (
    d: HierarchyPointNode<Node>,
    nodeList: HierarchyPointNode<Node>[]
  ) => {
    return nodeList.filter(k => k.data.id === d.data.id)[0]
  }

  drawTree() {
    // console.log(this.props.rootNode)

    // let recurse = (insideNode: Node) => {
    //   console.log(insideNode.id)

    //   if (!insideNode.children) return

    //   insideNode.children!.forEach(kid => {
    //     recurse(kid)
    //   })
    // }

    // recurse(this.props.rootNode)

    const hierarchy = d3.hierarchy<Node>(this.props.rootNode)

    const sorted = hierarchy
      .descendants()
      .sort((a, b) => b.data.label.length - a.data.label.length)

    // const maxRadius = this.props.linScale(this.props.rootNode.descCount)
    const maxRadius = 30

    const maxWidth = this.props.showLabels ? maxRadius * 3 : maxRadius * 1.5
    const maxHeight = this.props.showLabels ? maxRadius * 1.5 : maxRadius * 1.5

    const layout = d3.tree<Node>().nodeSize([maxWidth, maxHeight])
    const svg = d3.select(`#${this.props.identifier}thegroup`)
    const rootNode = layout(hierarchy)
    let nodeList = rootNode.descendants()

    nodeList = nodeList.filter(x => x.data.id !== -1)

    console.log(nodeList)

    let g = svg.selectAll("g.node")
    let node = g.data(nodeList, (d: any) => {
      let identifier = `NodeId${d.data.id}, TreeID:${d.data.treeId}`
      // console.log(identifier)
      return identifier
    })

    let nodeEnter = node
      .enter()
      .append("g")
      .attr("class", "node")
      .on("click", (d: HierarchyPointNode<Node>) => {
        this.props.nodeClickHandler(d.data)
      })

    nodeEnter
      .attr("transform", d => {
        let entering = d.parent
          ? `translate(${this.getPrevPos(d.parent).x},${
              this.getPrevPos(d.parent).y
            })`
          : ""

        return entering
      })
      .each(d => this.maybeFocus(d))
      .transition()
      .duration(this.props.duration)
      .attr("transform", d => `translate(${d.x},${d.y})`)

    nodeEnter.append("circle")

    nodeEnter
      .append("text")
      .style("fill-opacity", 1e-6)
      .attr("fill", "black")
      .attr("class", "decision")
      //   .attr("y", -maxHeight / 2)
      .attr("dy", ".35em")
      .attr("text-anchor", "middle")
      .text(d => {
        return this.props.showLabels ? d.data.id : ""
      })
      .transition()
      .duration(this.props.duration)
      .style("fill-opacity", 1)

    nodeEnter
      .append("text")
      .style("fill-opacity", 1e-6)
      .attr("y", d => {
        return (
          2 *
          Node.getRadius(
            d,
            this.props.linScale,
            this.props.minsize,
            this.props.diffParentId
          )
        )
      })
      .attr("class", "decCount")
      .attr("dy", ".35em")
      .attr("text-anchor", "middle")
      .text(d => (this.props.showLabels ? this.getDecCountMessage(d) : ""))
      .transition()
      .duration(this.props.duration)
      .style("fill-opacity", 1)

    this.updateCircles(nodeEnter)

    const nodeUpdate = node.each(d => {
      this.maybeFocus(d)
    })

    nodeUpdate
      .transition()
      .duration(this.props.duration)
      .attr("transform", (d: HierarchyPointNode<Node>) => {
        return `translate(${d.x},${d.y})`
      })

    nodeUpdate
      .select("text.decCount")
      .attr("y", d => {
        return (
          2 *
          Node.getRadius(
            d,
            this.props.linScale,
            this.props.minsize,
            this.props.diffParentId
          )
        )
      })
      .transition()
      .duration(this.props.duration)
      .text(d => (this.props.showLabels ? this.getDecCountMessage(d) : ""))
      .style("fill-opacity", d => (this.getDecCountMessage(d) === "" ? 0 : 1))

    nodeUpdate.select("text.decision").text(d => {
      return this.props.showLabels ? d.data.id : ""
    })

    this.updateCircles(nodeUpdate)

    const nodeExit = node.exit<HierarchyPointNode<Node>>()

    nodeExit
      .selectAll("text")
      .transition()
      .duration(this.props.duration)
      .style("fill-opacity", 1e-6)

    nodeExit
      .select("circle")
      .transition()
      .duration(this.props.duration)
      .attr("r", 0)

    nodeExit
      .transition()
      .duration(this.props.duration)
      .attr("transform", d => {
        let parent = this.getParentNode(d, nodeList)
        parent = parent ? parent : d.parent!

        let exiting = `translate(${parent.x},${parent.y})`
        return exiting
      })
      .remove()

    let p = svg.selectAll("path.link")

    let linkList = (rootNode
      .links()
      .filter(x => x.source.data.id !== -1) as any[]).map(x => {
      x.highlight = WhichTree.Both
      return x
    }) as Link[]

    const toHighlightLeft = linkList
      //   .filter(x => x.target.data.treeID === WhichTree.Left)
      .filter(x =>
        x.target.ancestors().find(y => y.data.treeId === WhichTree.Left)
      )
      .map(x => {
        let copy = cloneDeep(x)
        copy.highlight = WhichTree.Left
        return copy
      })

    const toHighlightRight = linkList
      .filter(x =>
        x.target.ancestors().find(y => y.data.treeId === WhichTree.Right)
      )
      .map(x => {
        let copy = cloneDeep(x)
        copy.highlight = WhichTree.Right
        return copy
      })

    linkList = linkList
      .concat(toHighlightLeft)
      .concat(toHighlightRight)
      .reverse()

    // console.log(linkList.filter(x => x.source.data.treeID !== WhichTree.Both))
    // console.log(toHighlight)
    // console.log(nodeList.filter(x => x.data.treeID !== WhichTree.Both))

    let link = p.data(
      linkList,
      (d: any) => `${d.target.data.id}${d.target.data.treeId}${d.highlight}`
    )

    const enterLink = link
      .enter()
      .insert("svg:path", "g")
      .classed("link", true)
      .classed("highlightLeft", d => {
        return d.highlight == WhichTree.Left
      })
      .classed("highlightRight", d => {
        return d.highlight == WhichTree.Right
      })
      .classed("red", d => {
        let solAncestorIds = this.getSolAncestorIds(d.target)
        return (
          (!solAncestorIds.includes(d.target.data.id) ||
            !this.props.solveable) &&
          d.highlight === WhichTree.Both
        )
      })
      .classed(
        "different",
        d =>
          d.target.data.id === d.source.data.id + 1 &&
          d.source.data.id === this.props.diffParentId &&
          this.props.diffParentId !== 0
      )
      .attr("d", d => {
        const origin = {
          x: this.getPrevPos(d.source).x,
          y: this.getPrevPos(d.source).y
        }
        return linkGenerator({ source: origin, target: origin })
      })
      .style("stroke-opacity", 0)
      .transition()
      .duration(this.props.duration)
      .style("stroke-opacity", 1)
      .attr("d", linkGenerator)

    link
      .classed("red", d => {
        let solAncestorIds = this.getSolAncestorIds(d.target)
        return (
          !solAncestorIds.includes(d.target.data.id) || !this.props.solveable
        )
      })
      .classed(
        "different",
        d =>
          d.target.data.id === d.source.data.id + 1 &&
          d.source.data.id === this.props.diffParentId &&
          this.props.diffParentId !== 0
        // this.props.diffParentIds.includes(d.source.data.id) &&
        // d.target.data.id === d.source.data.id + 1
      )
      .transition()
      .duration(this.props.duration)
      .attr("d", linkGenerator)
      .style("stroke-opacity", 1)

    link
      .exit<HierarchyPointLink<Node>>()
      .transition()
      .duration(this.props.duration)
      .style("stroke-opacity", 0)
      .attr("d", d => {
        let current = this.getNode(d.source, nodeList)

        current = current ? current : d.source

        const origin = { x: current.x, y: current.y }
        return linkGenerator({ source: origin, target: origin })
      })
      .remove()

    this.setState((prevState: State) => {
      let newMap = cloneDeep(prevState.oldPos)
      nodeList.forEach(d => {
        if (!newMap[d.data.id]) {
          newMap[d.data.id] = { x: -1, y: -1 }
        }

        newMap[d.data.id].x = d.x
        newMap[d.data.id].y = d.y
      })
      return { oldPos: newMap }
    })
  }

  makeGroup() {
    d3.select(`#${this.props.identifier} svg`)
      .call(this.zoom)
      .append("g")
      .attr("id", `${this.props.identifier}thegroup`)
  }

  componentDidMount() {
    this.makeGroup()
    this.drawTree()
    this.drawTree()
  }

  componentDidUpdate(prevProps: Props) {
    if (
      prevProps.selectedTreeId !== this.props.selectedTreeId ||
      prevProps.selected !== this.props.selected ||
      !isEqual(prevProps.rootNode, this.props.rootNode) ||
      prevProps.showLabels !== this.props.showLabels ||
      prevProps.diffParentId !== this.props.diffParentId
    ) {
      this.drawTree()
    }
  }

  render() {
    return (
      <div id={this.props.identifier} className="svg-container">
        <svg
          id="treeSVG"
          preserveAspectRatio="xMidYMid slice"
          viewBox={`0 0 ${this.props.width} ${this.props.height}`}
        ></svg>
      </div>
    )
  }
}
