import * as React from "react";
import * as ReactDOM from "react-dom";
import * as d3 from "d3";
import Node from "../modules/Node";
import { HierarchyPointLink, HierarchyPointNode, Selection } from "d3";
import { linkGenerator } from "../modules/Helper";

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
  nodeClickHandler: (d: Node) => void;
}

const zoom = d3.zoom<any, any>().on("zoom", function() {
  d3.select("#thegroup").attr("transform", d3.event.transform);
});

export default class TreeVis extends React.Component<Props, State> {
  // static whyDidYouRender = true;

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
      // node.y + 400
      node.y + this.props.height / 4
    );
  }

  getRadius(d: HierarchyPointNode<Node>): number {
    return (d.children ? d.children.length : 0) < d.data.childCount
      ? this.props.linScale(d.data.descCount)
      : this.props.minsize;
  }

  hasHiddenChildren(d: HierarchyPointNode<Node>): boolean {
    return d.data.childCount !== (d.children ? d.children.length : 0);
  }

  getDecCountMessage(d: HierarchyPointNode<Node>): string {
    if (this.hasHiddenChildren(d)) {
      return d.data.descCount + " nodes below";
    }
    return "";
  }

  updateCircles(selector: any) {
    let circle = selector.select("circle");

    circle
      .transition()
      .duration(this.props.duration)
      .attr("r", (d: HierarchyPointNode<Node>) => this.getRadius(d));

    circle.classed(
      "selected",
      (d: HierarchyPointNode<Node>) => d.data.id === this.props.selected
    );

    circle.classed("hasOthers", (d: HierarchyPointNode<Node>) =>
      this.hasHiddenChildren(d)
    );

    circle.classed(
      "red",
      (d: HierarchyPointNode<Node>) =>
        !this.props.solAncestorIds.includes(d.data.id) || !this.props.solveable
    );

    circle.classed(
      "solution",
      (d: HierarchyPointNode<Node>) => d.data.isSolution
    );
  }

  drawTree() {
    const hierarchy = d3.hierarchy<Node>(this.props.rootNode);
    const sorted = hierarchy
      .descendants()
      .sort((a, b) => b.data.label.length - a.data.label.length);

    const maxWidth = sorted[0].data.label.length * 10;
    const maxHeight = this.props.linScale(this.props.rootNode.descCount) * 3;

    const layout = d3.tree<Node>().nodeSize([maxWidth, maxHeight]);
    const svg = d3.select("#thegroup");
    const rootNode = layout(hierarchy);
    const nodeList = rootNode.descendants();

    // console.log(JSON.stringify(nodeList));
    // console.log(this.props.rootNode);
    // console.log(nodeList.map(d => d.data));

    let g = svg.selectAll("g.node");
    let node = g.data(nodeList, (d: any) => d.data.id);

    let nodeEnter = node
      .enter()
      .append("g")
      .attr("class", "node")
      .on("click", (d: HierarchyPointNode<Node>) => {
        this.props.nodeClickHandler(d.data);
      });

    nodeEnter
      .attr("transform", d =>
        d.parent ? `translate(${d.parent.x},${d.parent.y})` : ""
      )
      .transition()
      .duration(this.props.duration)
      .attr("transform", d => `translate(${d.x},${d.y})`);

    nodeEnter.append("circle");

    nodeEnter
      .append("text")
      .attr("fill", "black")
      .attr("y", -maxHeight / 2)
      .attr("x", d => {
        // if (d.parent && d.parent.children!.length > 1) {
        //   return (d.data.isLeftChild ? maxWidth : -maxWidth) / 16;
        // }
        return 0;
      })
      .attr("dy", ".35em")
      .attr("text-anchor", "middle")
      .text(d => {
        return d.data.label;
      });

    nodeEnter
      .append("text")
      .attr("y", d => {
        return 2 * this.getRadius(d);
      })
      .attr("class", "decCount")
      .attr("dy", ".35em")
      .attr("text-anchor", "middle")
      .text(d => this.getDecCountMessage(d));

    this.updateCircles(nodeEnter);

    const nodeUpdate = node.each(d => {
      if (d.data.id === this.props.selected) {
        this.focusNode(d);
      }
    });

    nodeUpdate
      .transition()
      .duration(this.props.duration)
      .attr(
        "transform",
        (d: HierarchyPointNode<Node>) => `translate(${d.x},${d.y})`
      );

    nodeUpdate
      .select("text.decCount")
      .attr("y", d => {
        return 2 * this.getRadius(d);
      })
      .text(d => this.getDecCountMessage(d));

    this.updateCircles(nodeUpdate);

    const nodeExit = node
      .exit<HierarchyPointNode<Node>>()
      .transition()
      .duration(this.props.duration)
      .attr("transform", d =>
        d.parent ? `translate(${d.parent.x},${d.parent.y})` : ""
      )
      .remove();

    nodeExit.select("circle").attr("r", 0);

    let p = svg.selectAll("path.link");

    const linkList = rootNode.links();
    let link = p.data(linkList, (d: any) => d.target.data.id);

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
      .classed("red", d => {
        return (
          !this.props.solAncestorIds.includes(d.target.data.id) ||
          !this.props.solveable
        );
      })
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
    this.drawTree();
  }

  render() {
    return (
      <div id="tree">
        <svg width={this.props.width} height={this.props.height}></svg>
      </div>
    );
  }
}
