import * as React from "react"
import * as ReactDOM from "react-dom"
import * as d3 from "d3"
import Node from "../modules/Node"
import { HierarchyPointLink, HierarchyPointNode, Selection } from "d3"
import { linkGenerator } from "../modules/Helper"

interface State {}

interface Props {
  id: string
  identifier: string
  width: number
  height: number
  rootNode: Node
  duration: number
  selected: number
  solAncestorIds: number[]
  solveable: boolean
  linScale: any
  minsize: number
  nodeClickHandler: (d: Node) => void
}

export default class TreeVis extends React.Component<Props, State> {
  // static whyDidYouRender = true;
  zoom: any

  constructor(props: Props) {
    super(props)
    this.state = {}

    console.log(this.props)
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
    // const ratio = 1290 / 465.45
    // console.log("ratio, ", ratio)

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
    return d.data.childCount !== (d.children ? d.children.length : 0)
  }

  getDecCountMessage(d: HierarchyPointNode<Node>): string {
    if (this.hasHiddenChildren(d)) {
      return d.data.descCount + " nodes below"
    }
    return ""
  }

  maybeFocus(d: HierarchyPointNode<Node>): void {
    if (d.data.id === this.props.selected) {
      this.focusNode(d)
    }
  }

  updateCircles(selector: any) {
    let circle = selector.select("circle")

    circle
      .transition()
      .duration(this.props.duration)
      .attr("r", (d: HierarchyPointNode<Node>) =>
        Node.getRadius(d, this.props.linScale, this.props.minsize)
      )

    circle.classed(
      "selected",
      (d: HierarchyPointNode<Node>) => d.data.id === this.props.selected
    )

    circle.classed("hasOthers", (d: HierarchyPointNode<Node>) =>
      this.hasHiddenChildren(d)
    )

    circle.classed(
      "red",
      (d: HierarchyPointNode<Node>) =>
        !this.props.solAncestorIds.includes(d.data.id) || !this.props.solveable
    )

    circle.classed(
      "solution",
      (d: HierarchyPointNode<Node>) => d.data.isSolution
    )
  }

  drawTree() {
    const hierarchy = d3.hierarchy<Node>(this.props.rootNode)
    const sorted = hierarchy
      .descendants()
      .sort((a, b) => b.data.label.length - a.data.label.length)

    const maxWidth = sorted[0].data.label.length * 10
    const maxHeight = this.props.linScale(this.props.rootNode.descCount) * 3

    const layout = d3.tree<Node>().nodeSize([maxWidth, maxHeight])
    const svg = d3.select(`#${this.props.identifier}thegroup`)
    const rootNode = layout(hierarchy)
    const nodeList = rootNode.descendants()

    // console.log(JSON.stringify(nodeList));
    // console.log(this.props.rootNode);
    // console.log(nodeList.map(d => d.data));

    let g = svg.selectAll("g.node")
    let node = g.data(nodeList, (d: any) => d.data.id)

    let nodeEnter = node
      .enter()
      .append("g")
      .attr("class", "node")
      .on("click", (d: HierarchyPointNode<Node>) => {
        this.props.nodeClickHandler(d.data)
      })

    nodeEnter
      .attr("transform", d =>
        d.parent ? `translate(${d.parent.x},${d.parent.y})` : ""
      )
      .each(d => this.maybeFocus(d))
      .transition()
      .duration(this.props.duration)
      .attr("transform", d => `translate(${d.x},${d.y})`)

    nodeEnter.append("circle")

    nodeEnter
      .append("text")
      .attr("fill", "black")
      .attr("y", -maxHeight / 2)
      .attr("dy", ".35em")
      .attr("text-anchor", "middle")
      .text(d => {
        return d.data.label
      })

    nodeEnter
      .append("text")
      .attr("y", d => {
        return 2 * Node.getRadius(d, this.props.linScale, this.props.minsize)
      })
      .attr("class", "decCount")
      .attr("dy", ".35em")
      .attr("text-anchor", "middle")
      .text(d => this.getDecCountMessage(d))

    this.updateCircles(nodeEnter)

    const nodeUpdate = node.each(d => {
      this.maybeFocus(d)
    })

    nodeUpdate
      .transition()
      .duration(this.props.duration)
      .attr(
        "transform",
        (d: HierarchyPointNode<Node>) => `translate(${d.x},${d.y})`
      )

    nodeUpdate
      .select("text.decCount")
      .attr("y", d => {
        return 2 * Node.getRadius(d, this.props.linScale, this.props.minsize)
      })
      .text(d => this.getDecCountMessage(d))

    this.updateCircles(nodeUpdate)

    const nodeExit = node
      .exit<HierarchyPointNode<Node>>()
      .transition()
      .duration(this.props.duration)
      .attr("transform", d =>
        d.parent ? `translate(${d.parent.x},${d.parent.y})` : ""
      )
      .remove()

    nodeExit.select("circle").attr("r", 0)

    let p = svg.selectAll("path.link")

    const linkList = rootNode.links()
    let link = p.data(linkList, (d: any) => d.target.data.id)

    link
      .enter()
      .insert("svg:path", "g")
      .classed("link", true)
      .classed("red", d => {
        return (
          !this.props.solAncestorIds.includes(d.target.data.id) ||
          !this.props.solveable
        )
      })
      .attr("d", d => {
        const origin = { x: d.source.x, y: d.source.y }
        return linkGenerator({ source: origin, target: origin })
      })
      .transition()
      .duration(this.props.duration)
      .attr("d", linkGenerator)

    link
      .classed("red", d => {
        return (
          !this.props.solAncestorIds.includes(d.target.data.id) ||
          !this.props.solveable
        )
      })
      .transition()
      .duration(this.props.duration)
      .attr("d", linkGenerator)

    link
      .exit<HierarchyPointLink<Node>>()
      .transition()
      .duration(this.props.duration)
      .attr("d", d => {
        const origin = { x: d.source.x, y: d.source.y }
        return linkGenerator({ source: origin, target: origin })
      })
      .remove()
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

  componentDidUpdate() {
    this.drawTree()
  }

  render() {
    return (
      <div id={this.props.identifier} className="svg-container">
        <svg
          preserveAspectRatio="xMidYMid meet"
          // viewBox="0 0 1290 465.45"
          viewBox={`0 0 ${this.props.width} ${this.props.height}`}
          // viewBox={`0 0 600 400`}
          // className="svg-content-responsive"
        ></svg>
      </div>
    )
  }
}
