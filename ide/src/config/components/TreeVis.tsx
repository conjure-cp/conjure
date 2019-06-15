import * as React from "react";
import * as ReactDOM from "react-dom";
import * as d3 from "d3";
import Node from "../modules/Node";
import { HierarchyPointLink, HierarchyPointNode, Selection } from "d3";

interface State {}

interface Props {
  id: string;
  width: number;
  height: number;
  rootNode: Node;
  duration: number;
  selected: number;
  solAncestorIds: number[];
  solveable: boolean;
  linScale: any;
  minsize: number;
}

const linkGenerator = d3
  .linkVertical<any, Node>()
  .x(function(d: Node) {
    return d.x;
  })
  .y(function(d: Node) {
    return d.y;
  });

const zoom = d3.zoom<any, any>().on("zoom", function() {
  d3.select("#thegroup").attr("transform", d3.event.transform);
});

export default class TreeVis extends React.Component<Props, State> {
  static whyDidYouRender = true;

  constructor(props: Props) {
    super(props);
    this.state = {};
  }

  focusNode(node: HierarchyPointNode<Node>) {
    zoom.translateTo(
      d3
        .select("#tree svg")
        .transition()
        .duration(this.props.duration),
      node.x,
      node.y
    );
  }

  updateCircles(selector: any) {
    let circle = selector.select("circle");

    circle
      .transition()
      .duration(this.props.duration)
      .attr("r", (d: HierarchyPointNode<Node>) => {
        return (d.children ? d.children.length : 0) < d.data.childCount
          ? this.props.linScale(d.data.descCount)
          : this.props.minsize;
      });

    circle.classed(
      "selected",
      (d: HierarchyPointNode<Node>) => d.data.id === this.props.selected
    );

    circle.classed(
      "hasOthers",
      (d: HierarchyPointNode<Node>) =>
        d.data.childCount !== (d.children ? d.children.length : 0)
    );

    circle.classed(
      "red",
      (d: HierarchyPointNode<Node>) =>
        !this.props.solAncestorIds.includes(d.data.id)
    );

    circle.classed(
      "solution",
      (d: HierarchyPointNode<Node>) => d.data.isSolution
    );
  }

  drawTree() {
    const layout = d3.tree<Node>().nodeSize([50, 50]);
    const svg = d3.select("#thegroup");
    const rootNode = layout(d3.hierarchy<Node>(this.props.rootNode));
    const nodeList = rootNode.descendants();

    let g = svg.selectAll("g.node");
    let node = g.data(nodeList);

    let nodeEnter = node
      .enter()
      .append("g")
      .attr("class", "node");

    nodeEnter.append("circle");

    nodeEnter
      .attr("transform", d =>
        d.parent ? `translate(${d.parent.x},${d.parent.y})` : ""
      )
      .transition()
      .duration(this.props.duration)
      .attr("transform", d => `translate(${d.x},${d.y})`);

    this.updateCircles(nodeEnter);

    const nodeUpdate = node.each(d => {
      if (d.data.id === this.props.selected) {
        this.focusNode(d);
      }
    });

    this.updateCircles(nodeUpdate);

    const nodeExit = node.exit<HierarchyPointNode<Node>>();

    nodeExit
      .transition()
      .duration(this.props.duration)
      .attr("transform", d =>
        d.parent ? `translate(${d.parent.x},${d.parent.y})` : ""
      )
      .remove();

    nodeExit
      .transition()
      .duration(this.props.duration)
      .select("circle")
      .attr("r", 0);

    let p = svg.selectAll("path.link");

    const linkList = rootNode.links();
    let link = p.data(linkList);

    link
      .enter()
      .insert("svg:path", "g")
      .classed("link", true)
      .classed("red", d => {
        return (
          !this.props.solAncestorIds.includes(d.target.data.id) ||
          !this.props.solveable
        );
      })
      .attr("d", d => {
        const origin = { x: d.source.x, y: d.source.y };
        return linkGenerator({ source: origin, target: origin });
      })
      .transition()
      .duration(this.props.duration)
      .attr("d", linkGenerator);

    link
      .transition()
      .duration(this.props.duration)
      .attr("d", linkGenerator);

    link
      .exit<HierarchyPointLink<Node>>()
      .transition()
      .duration(this.props.duration)
      .attr("d", d => {
        const origin = { x: d.source.x, y: d.source.y };
        return linkGenerator({ source: origin, target: origin });
      })
      .remove();
  }

  makeGroup() {
    d3.select("#tree svg")
      .call(zoom)
      .append("g")
      .attr("id", "thegroup");
  }

  componentDidMount() {
    this.makeGroup();

    this.drawTree();
    this.drawTree();
  }

  componentDidUpdate(prevProps: Props) {
    // console.log("TreeVis props", JSON.stringify(this.props));

    // if (prevProps.id !== this.props.id) {
    //   console.log("CLEARING");
    //   let myNode = document.getElementById("thegroup");
    //   myNode.innerHTML = "";
    // } else {
    //   console.log("not cleaing");
    // }
    this.drawTree();
  }

  render() {
    return (
      <div id="tree">
        This is the tree
        <svg width={this.props.width} height={this.props.height}></svg>
      </div>
    );
  }
}
