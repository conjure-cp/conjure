import * as React from "react"
import * as ReactDOM from "react-dom"
import * as d3 from "d3"
import Node from "../modules/Node"
import { HierarchyPointLink, HierarchyPointNode, Selection } from "d3"
import { linkGenerator } from "../modules/TreeHelper"
import { isEqual, reduce, cloneDeep } from "lodash"
import { runInThisContext } from "vm"

type num2num = Record<number, { x: number; y: number }>

interface Props {
  first: boolean
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
  storeNodePrevPos: (list: HierarchyPointNode<Node>[]) => void
}

interface State {
  oldPos: num2num
}

export default class TreeVis extends React.Component<Props, State> {
  // static whyDidYouRender = true
  zoom: any

  constructor(props: Props) {
    super(props)
    this.state = { oldPos: {} }

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

  getPrevPos = (d: HierarchyPointNode<Node>): { x: number; y: number } => {
    return this.state.oldPos[d.data.id]
      ? this.state.oldPos[d.data.id]
      : { x: -1, y: -1 }
  }

  printPos = (nodeList: HierarchyPointNode<Node>[]) => {
    console.log(
      nodeList
        .filter(d => d.data.id === 28)
        .map(d =>
          this.state.oldPos[28]
            ? `${d.x}, ${d.y} \n ${this.state.oldPos[28].x},${this.state.oldPos[28].y}`
            : undefined
        )
    )
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

  drawTree(prevProps: Props, first?: boolean) {
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

    console.log(nodeList.filter(d => d.data.id === 28)[0])

    // console.log(JSON.stringify(nodeList));
    // console.log(this.props.rootNode);
    // console.log(nodeList.map(d => d.data));

    this.printPos(nodeList)

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
      .attr("transform", d => {
        // console.log("")

        let entering = d.parent
          ? `translate(${this.getPrevPos(d.parent).x},${
              this.getPrevPos(d.parent).y
            })`
          : ""

        if (d.data.id === 29) {
          // console.log("```````````````````````")
          // console.log("entering ", entering)
          // console.log(this.state.oldPos[d.parent!.data.id])
        }

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
      // .attr("y", -maxHeight / 2)
      .attr("dy", ".35em")
      .attr("text-anchor", "middle")
      .text(d => {
        // return d.data.label
        return `(${d.x},${d.y})
                (${this.getPrevPos(d).x}, ${this.getPrevPos(d).y})`
      })
      .transition()
      .duration(this.props.duration)
      .style("fill-opacity", 1)

    nodeEnter
      .append("text")
      .style("fill-opacity", 1e-6)
      .attr("y", d => {
        return 2 * Node.getRadius(d, this.props.linScale, this.props.minsize)
      })
      .attr("class", "decCount")
      .attr("dy", ".35em")
      .attr("text-anchor", "middle")
      .text(d => this.getDecCountMessage(d))
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
        return 2 * Node.getRadius(d, this.props.linScale, this.props.minsize)
      })
      .transition()
      .duration(this.props.duration)
      .text(d => this.getDecCountMessage(d))
      .style("fill-opacity", d =>
        this.getDecCountMessage(d) === "" ? 1e-6 : 1
      )

    nodeUpdate.select("text.decision").text(d => {
      // return d.data.label
      return `(${d.x},${d.y})
                (${this.getPrevPos(d).x}, ${this.getPrevPos(d).y})`
    })

    this.updateCircles(nodeUpdate)

    const nodeExit = node.exit<HierarchyPointNode<Node>>()

    nodeExit
      .selectAll("text")
      .transition()
      .duration(this.props.duration)
      .style("fill-opacity", 1e-6)

    nodeExit.select("text.decision").text(d => {
      // return d.data.label
      return `(${d.x},${d.y})
                (${this.getPrevPos(d).x}, ${this.getPrevPos(d).y})`
    })

    nodeExit
      .select("circle")
      .transition()
      .duration(this.props.duration)
      .attr("r", 0)

    nodeExit
      .transition()
      .duration(this.props.duration)
      .attr("transform", d => {
        // let exiting = d.parent
        //   ? `translate(${this.getPrevPos(d.parent).x},${
        //       this.getPrevPos(d.parent).y
        //     })`

        // console.log(d.parent)

        // console.log(
        //   d.parent!.x === this.getPrevPos(d.parent!).x &&
        //     d.parent!.y === this.getPrevPos(d.parent!).y
        // )

        // this.printPos(nodeList)
        // let parent = nodeList.filter(k => k.data.id === d.parent!.data.id)[0]

        let parent = this.getParentNode(d, nodeList)

        parent = parent ? parent : d.parent!

        //   : ""
        let exiting = `translate(${parent.x},${parent.y})`

        // console.log(this.state.oldPos[d.parent.data.id])
        // // console.log(this.state.oldPos[28])
        // console.log("exiting ", exiting)

        // console.log(`${d.parent.x}, ${d.parent.y}`)
        // console.log(d.parent)

        return exiting

        // return d.parent && this.state.oldPos[d.parent.data.id]
        //   ? `translate(${this.state.oldPos[d.parent.data.id].x},${this.state.oldPos[d.parent.data.id].y})`
        //   : ""

        // return d.parent
        //   ? `translate(${d.parent.data.x0},${d.parent.data.y0})`
        //   : ""
      })
      .remove()

    let p = svg.selectAll("path.link")

    const linkList = rootNode.links()
    let link = p.data(linkList, (d: any) => d.target.data.id)

    const enterLink = link
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
        // let parent = d.source.parent
        //   ? this.getParentNode(d.source, nodeList)
        //   : { x: 0, y: 0 }

        let current = this.getNode(d.source, nodeList)

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
        return (
          !this.props.solAncestorIds.includes(d.target.data.id) ||
          !this.props.solveable
        )
      })
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
        // let parent = this.getParentNode(d.target, nodeList)

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

    // this.setState((prevState: State) => {
    //   let newXMap = cloneDeep(prevState.id2PrevX)
    //   let newYMap = cloneDeep(prevState.id2PrevY)

    //   nodeList.forEach(d => {
    //     newXMap[d.data.id] = d.x
    //     newYMap[d.data.id] = d.y
    //   })

    //   return { id2PrevX: newXMap, id2PrevY: newYMap }
    // })

    // function getObjectDiff(obj1: any, obj2: any) {
    //   const diff = Object.keys(obj1).reduce((result, key) => {
    //     if (!obj2.hasOwnProperty(key)) {
    //       result.push(key)
    //     } else if (isEqual(obj1[key], obj2[key])) {
    //       const resultKeyIndex = result.indexOf(key)
    //       result.splice(resultKeyIndex, 1)
    //     }
    //     return result
    //   }, Object.keys(obj2))

    //   return diff
    // }

    // let childrenDifferent = false

    // if (!isEqual(this.props.rootNode, prevProps.rootNode)) {
    //   function recurse(node1: Node, node2: Node) {
    //     if (node1.children) {
    //       node1.children.forEach((kid, i) => {
    //         if (!node2.children) {
    //           // console.error("CHILDREN ", node1, node2)
    //           childrenDifferent = true
    //           return
    //         }

    //         recurse(kid, node2.children![i])
    //       })
    //     }

    //     if (!isEqual(node1, node2)) {
    //       // console.error(node1, node2)
    //     }
    //   }

    //   recurse(this.props.rootNode, prevProps.rootNode)
    // console.log(getObjectDiff(this.props.rootNode, prevProps.rootNode))

    // console.log(getObjectDiff(this.props.rootNode, prevProps.rootNode))
    // if (
    //   isEqual(getObjectDiff(this.props.rootNode, prevProps.rootNode), [
    //     "children"
    //   ]) ||
    //   first
    // ) {
    //   console.log("HERE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
    //   this.props.storeNodePrevPos(nodeList)
    // }
  }

  makeGroup() {
    d3.select(`#${this.props.identifier} svg`)
      .call(this.zoom)
      .append("g")
      .attr("id", `${this.props.identifier}thegroup`)
  }

  componentDidMount() {
    this.makeGroup()
    this.drawTree(this.props)
    this.drawTree(this.props, true)
  }

  componentDidUpdate(prevProps: Props) {
    if (
      prevProps.selected !== this.props.selected ||
      !isEqual(prevProps.rootNode, this.props.rootNode)
    ) {
      this.drawTree(prevProps)
    }
  }

  render() {
    return (
      <div id={this.props.identifier} className="svg-container">
        <svg
          id="treeSVG"
          preserveAspectRatio="xMidYMid slice"
          // viewBox="0 0 1290 465.45"
          viewBox={`0 0 ${this.props.width} ${this.props.height}`}
          // viewBox={`0 0 600 400`}
          // className="svg-content-responsive"
        ></svg>
      </div>
    )
  }
}
