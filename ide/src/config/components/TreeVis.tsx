import * as React from "react";
import * as ReactDOM from "react-dom";
import * as d3 from "d3";
import Node from "../modules/Node";
import { HierarchyPointLink, HierarchyPointNode } from "d3";

interface State {}

interface Props {
  width: number;
  height: number;
  rootNode: Node;
  duration: number;
  selected: number;
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
  d3.select("#tree svg g").attr("transform", d3.event.transform);
});

export default class TreeVis extends React.Component<Props, State> {
  constructor(props: Props) {
    super(props);
    this.state = {};
  }

  focusNode(node: HierarchyPointNode<Node>) {
    let x = -node.x;
    let y = -node.y;

    zoom.translateTo(
      d3
        .select("#tree svg")
        .transition()
        .duration(750),
      x,
      y
    );
  }

  drawTree() {
    const layout = d3.tree<Node>().nodeSize([50, 50]);
    const svg = d3.select("#tree svg g");
    const rootNode = layout(d3.hierarchy<Node>(this.props.rootNode));
    const nodeList = rootNode.descendants();
    let g = svg.selectAll("g.node");
    let node = g.data(nodeList);

    let nodeEnter = node
      .enter()
      .append("g")
      .attr("class", "node");

    nodeEnter
      .attr("transform", d =>
        d.parent ? `translate(${d.parent.x},${d.parent.y})` : ""
      )
      .transition()
      .duration(this.props.duration)
      .attr("transform", d => `translate(${d.x},${d.y})`);

    nodeEnter
      .append("svg:circle")
      .attr("r", 6)
      .attr("fill", "green");

    node.select("circle").classed("selected", d => {
      return d.data.id === this.props.selected;
    });

    node.each(d => {
      if (d.data.id === this.props.selected) {
        this.focusNode(d);
      }
    });

    node
      .transition()
      .duration(this.props.duration)
      .attr("transform", d => `translate(${d.x},${d.y})`);

    node
      .exit<HierarchyPointNode<Node>>()
      .transition()
      .duration(this.props.duration)
      .attr("transform", d =>
        d.parent ? `translate(${d.parent.x},${d.parent.y})` : ""
      )
      .remove();

    let p = svg.selectAll("path.link");

    const linkList = rootNode.links();
    let link = p.data(linkList);

    link
      .enter()
      .insert("svg:path", "g")
      .attr("class", "link")
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

  componentDidMount() {
    let svg = d3
      .select("#tree")
      .append("svg")
      .attr("width", this.props.width)
      .attr("height", this.props.height)
      .call(zoom)
      .append("g");

    this.drawTree();
  }

  componentDidUpdate() {
    this.drawTree();
  }

  render() {
    return <div id="tree">This is the tree</div>;
  }
}
