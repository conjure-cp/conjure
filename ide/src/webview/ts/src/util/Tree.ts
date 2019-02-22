declare var d3: any;
import Globals from './Globals';
import Node from './Node';

export default class Tree {

    public static gap = 100;
    public static duration = 750;
    public static viewerWidth = $(document).width();
    public static viewerHeight = $(document).height();
    public static margin = { top: 40, right: 30, bottom: 50, left: 30 };
    public static width = Tree.viewerWidth! - Tree.margin.left - Tree.margin.right;
    public static height = Tree.viewerHeight! - Tree.margin.top - Tree.margin.bottom;
    public static tree = d3.layout.tree().size([Tree.height, Tree.width]);

    public static zoom = d3.behavior.zoom()
        .on("zoom", Tree.zoomed);

    public static svg = d3.select("#tree")
        .append("svg")
        .call(Tree.zoom)
        .attr("width", Tree.viewerWidth)
        .attr("height", Tree.viewerHeight)
        .append("g");

    public static diagonal = d3.svg.diagonal()
        .projection((d: any) => {
            return [d.x, d.y];
        });

    public static zoomed() {
        Tree.svg.attr("transform", "translate(" + d3.event.translate + ")" + " scale(" + d3.event.scale + ")");
    }

    public static focusNode = (node: any) => {
        let scale = Tree.zoom.scale();
        let x = -node.x * scale;
        let y = -node.y * scale;

        x += Tree.width / 3;
        y += Tree.height / 2;

        d3.select('g').transition()
            .duration(Tree.duration)
            .attr("transform", "translate(" + x + "," + y + ")scale(" + scale + ")");
        Tree.zoom.translate([x, y]);
    }

    public static selectNode = (nodeId: number) => {
        Globals.s.selectedId = nodeId;

        let allCircles = ".node circle";
        d3.selectAll(allCircles).classed("selected", false);
        let s = "#node" + nodeId + " circle";
        d3.select(s).classed("selected", true);

        Tree.focusNode(Globals.s.id2Node[nodeId]);

        if (!Globals.s.frozen) {
            Globals.loadDomains();
        }
    }

    private static fillCircle(node: Node) {
        let s = "#node" + node.id + " circle";

        let domElement = d3.select(s);
        domElement.classed("hasOthers red", false);

        let childLength = 0;
        if (node.children) {
            childLength = node.children.length;
        }

        if (Globals.s.id2ChildIds[node.id]) {
            if (childLength < Globals.s.id2ChildIds[node.id].length) {

                if (!Globals.s.correctPath.includes(node.id)) {

                    domElement.classed("red", true);

                }
                domElement.classed("hasOthers", true);
            }
        }
    }

    public static update(source: Node) {

        let nodes = Tree.tree.nodes(Globals.s.id2Node[Globals.s.rootId]).reverse(),
            links = Tree.tree.links(nodes);

        nodes.forEach((node: Node) => { node.y = node.depth * Tree.gap; });

        let node = Tree.svg.selectAll("g.node")
            .data(nodes, (node: Node) => { return node.id; });

        let nodeEnter = node.enter().append("g")
            .attr("class", "node")
            .attr("id", (node: Node) => {
                return "node" + node.id;
            })
            .attr("transform", (node: Node) => {
                let parent = Globals.s.id2Node[node.id].parent;
                if (parent) {
                    return "translate(" + parent.x + "," + parent.y + ")";
                }
            })
            .on("click", (node: Node) => {
                Tree.selectNode(node.id);
            });

        nodeEnter.append("circle")
            .attr("r", 1e-6);

        nodeEnter.append("text")
            .attr("y", () => {
                return -50;
            })
            .attr("dy", ".35em")
            .attr("text-anchor", "middle")
            .text((node: Node) => { return node.name; })
            .style("fill-opacity", 1e-6);

        let nodeUpdate = node.transition()
            .duration(Tree.duration)
            .attr("transform", (node: Node) => { return "translate(" + node.x + "," + node.y + ")"; });

        nodeUpdate.select("circle")
            .attr("r", 10)
            .each((d: Node) => { Tree.fillCircle(d); });

        nodeUpdate.select("text")
            .style("fill-opacity", 1);

        let nodeExit = node.exit().transition()
            .duration(Tree.duration)
            .attr("transform", () => { return "translate(" + source.x + "," + source.y + ")"; })
            .remove();
        nodeExit.select("circle")
            .attr("r", 1e-6);
        nodeExit.select("text")
            .style("fill-opacity", 1e-6);

        let link = Tree.svg.selectAll("path.link")
            .data(links, (link: any) => { return link.target.id; });

        link.enter().insert("path", "g")
            .attr("class", (link: any) => {
                if (Globals.s.correctPath.includes(link.target.id)) {
                    return "link";
                }
                return "link red";
            })
            .attr("d", (d: any) => {
                let o = { x: d.source.x, y: d.source.y };
                return Tree.diagonal({ source: o, target: o });
            })
            .style("stroke-opacity", 1e-6);

        link.transition()
            .duration(Tree.duration)
            .attr("d", Tree.diagonal)
            .style("stroke-opacity", 1);

        link.exit().transition()
            .duration(Tree.duration)
            .attr("d", (d: any) => {
                let o = { x: d.source.x, y: d.source.y };
                return Tree.diagonal({ source: o, target: o });
            })
            .remove();

        nodes.forEach((node: Node) => {
            node.x0 = node.x;
            node.y0 = node.y;
        });
    }
}