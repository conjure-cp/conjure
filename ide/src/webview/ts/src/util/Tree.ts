declare var d3: any;
import Globals from './Globals';
import Node from './Node';

export default class Tree {

    public static gap = 100;
    public static duration = 500;
    public static viewerWidth = $(document).width();
    public static viewerHeight = $(document).height();
    public static margin = { top: 40, right: 30, bottom: 50, left: 30 };
    public static width = Tree.viewerWidth! - Tree.margin.left - Tree.margin.right;
    public static height = Tree.viewerHeight! - Tree.margin.top - Tree.margin.bottom;
    public static tree = d3.layout.tree().size([Tree.height, Tree.width]).nodeSize([300, 0]);

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

    public static focusNode = (node: Node) => {
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

        Globals.lv.updatePanelTitle();
    }


    private static fillCircle(node: Node) {

        // console.log(node.isSolution)

        let s = "#node" + node.id + " circle";

        let domElement = d3.select(s);
        domElement.classed("hasOthers red", false);

        // if (Globals.s.solNodIds.includes(node.id)) {
        if (node.isSolution) {
            domElement.classed("solution", true);
        }

        if (Node.hasMoreChildren(node)) {

            if (Globals.s.solAncestorIds.includes(node.id) && Globals.s.solNodIds.length > 0) {
                domElement.classed("hasOthers", true);
            }

            else {
                domElement.classed("hasOthers red", true);
            }

        }

        // domElement.attr("r", Node.calculateRadius(node));
    }

    public static getDecLabelHeight(node: Node) {
        return Node.calculateRadius(node) + 13;
    }

    public static update(source: Node) {

        // let nodes = Tree.tree.nodes(Globals.s.id2Node[Globals.s.rootId]).reverse(),
        let nodes = Tree.tree.nodes(Globals.s.id2Node[Globals.s.rootId]),
            links = Tree.tree.links(nodes);

        // console.log(nodes);

        nodes.forEach((node: Node) => { node.y = node.depth * Tree.gap; });

        let node = Tree.svg.selectAll("g.node")
            .data(nodes, (node: Node) => { return node.id; });

        let nodeEnter = node.enter().append("g")
            .attr("class", "node")
            .attr("id", (node: Node) => {
                return "node" + node.id;
            })
            .attr("transform", (node: Node) => {
                // console.log(node.parent);
                let parent = node.parent;
                if (parent) {
                    return "translate(" + parent.x + "," + parent.y + ")";
                }
            })
            .on("click", (node: Node) => {
                Tree.selectNode(node.id);
            })
            .each((d: Node) => {
                if (!Globals.s.id2Node[d.id]) {
                    Globals.s.id2Node[d.id] = d;
                }
                if (d.isSolution) {
                    Globals.s.solNodIds.push(d.id);
                }
            });

        nodeEnter.append("circle")
            .attr("r", (node: Node) => { return Node.calculateRadius(node); });


        nodeEnter.append("text")
            .attr("y", () => {
                return -50;
            })
            .attr("dy", ".35em")
            .attr("text-anchor", "middle")
            .text((node: Node) => {

                if ($("#labels").prop("checked") === true) {
                    return node.prettyLabel;
                }
                else {
                    return node.label;
                }
            })
            .style("fill-opacity", 1e-6);

        nodeEnter.append("text")
            .attr("y", (node: Node) => {
                return Tree.getDecLabelHeight(node);
            })
            .attr("class", "decCount")
            .attr("dy", ".35em")
            .attr("text-anchor", "middle")
            .text((node: Node) => {
                if (Node.hasMoreChildren(node)) {
                    return node.decCount;
                }
            });

        let nodeUpdate = node.transition()
            .duration(Tree.duration)
            .attr("transform", (node: Node) => { return "translate(" + node.x + "," + node.y + ")"; });

        nodeUpdate.select("circle")
            // .attr("r", 10)
            // .transition()
            // .duration(10)
            .attr("r", (node: Node) => {
                // let s = "#node" + node.id + " circle";

                // let domElement = d3.select(s);

                // console.log(circle);

                return Node.calculateRadius(node);

            })
            .attr("class", (node: Node) => {
                
                let res = "";

                if (Globals.s.selectedId === node.id){
                    res += "selected ";
                }

                if (node.isSolution) {
                    return res + " solution";
                }

                if (Node.hasMoreChildren(node)) {

                    if (Globals.s.solAncestorIds.includes(node.id) && Globals.s.solNodIds.length > 0) {
                        return res + " hasOthers";
                    }

                    else {
                       return  res + " hasOthers red";
                    }

                }


            });


        // .each((d: Node) => { Tree.fillCircle(d); });

        nodeUpdate.select("text")
            .style("fill-opacity", 1);

        nodeUpdate.select("text.decCount")
            .attr("y", (node: Node) => {
                return Tree.getDecLabelHeight(node);
            })
            .text((node: Node) => {
                if (Node.hasMoreChildren(node)) {
                    return node.decCount;
                }
            });


        let nodeExit = node.exit().transition()
            .duration(Tree.duration)
            .attr("transform", () => { return "translate(" + source.x + "," + source.y + ")"; })
            .remove();
        // nodeExit.select("circle")
        //     .attr("r", 1e-6);
        nodeExit.select("text")
            .style("fill-opacity", 1e-6);

        let link = Tree.svg.selectAll("path.link")
            .data(links, (link: any) => { return link.target.id; });

        link.enter().insert("path", "g")
            .attr("class", (link: any) => {
                if (Globals.s.solAncestorIds.includes(link.target.id) && Globals.s.solNodIds.length > 0) {
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