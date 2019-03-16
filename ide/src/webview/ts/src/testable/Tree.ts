declare var d3: any;
import Globals from './Globals';
import State from './State';
import Node from './Node';

export default class Tree {

    public static duration = 500;
    public static viewerWidth = $(document).width();
    public static viewerHeight = $(document).height();
    public static margin = { top: 40, right: 30, bottom: 50, left: 30 };
    public static width = Tree.viewerWidth! - Tree.margin.left - Tree.margin.right;
    public static height = Tree.viewerHeight! - Tree.margin.top - Tree.margin.bottom;
    public static nodeHeight = 150;
    public static tree = d3.layout.tree().size([Tree.height, Tree.width]).nodeSize([200, 0]);

    public static zoom = d3.behavior.zoom().on("zoom", Tree.zoomed);
    public static svg = Tree.makeSvg("#tree", Tree.zoom, Tree.viewerWidth, Tree.viewerWidth);

    public static diagonal = d3.svg.diagonal()
        .projection((d: any) => {
            return [d.x, d.y];
        });

    public static makeSvg(selector: string, zoomFunc: any, width: number, height: number) {
        return d3.select(selector)
            .append("svg")
            .call(zoomFunc)
            .attr("id", "theTree")
            .attr("width", width)
            .attr("height", height)
            .append("g");
    }

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
        State.selectedId = nodeId;

        let allCircles = ".node circle";
        d3.selectAll(allCircles).classed("selected", false);
        let s = "#node" + nodeId + " circle";
        // console.log("selecting " + nodeId);
        d3.select(s).classed("selected", true);
        Tree.focusNode(State.id2Node[nodeId]);

        if (!State.frozen) {
            Globals.loadDomains(Globals.vscode);
        }

        Globals.lv.updatePanelTitle();
    }

    public static appendCircle(selection: any) {
        selection.append("circle")
            .attr("r", (node: Node) => { return Node.calculateRadius(node); });
    }

    public static getNodeList(node: Node) {
        let nodeList = Tree.tree.nodes(node);
        nodeList.forEach((node: Node) => { node.y = node.depth * Tree.nodeHeight; });
        return nodeList;
    }

    public static enterNodes(selection: any) {
        return selection.enter().append("g")
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
                if (!State.id2Node[d.id]) {
                    State.id2Node[d.id] = d;
                }
            });
    }

    public static appendLabel(selection: any) {
        selection.append("text")
            .attr("y", () => {
                return -Tree.nodeHeight / 2;
            })
            .attr("dy", ".35em")
            .attr("text-anchor", "middle")
            .text((node: Node) => {

                if ($("#labels").is(":checked")) {
                    // console.log("CHECKDED")
                    // console.log($("#labels"))
                    return node.prettyLabel;
                }
                else {
                    return node.label;
                }
            })
            .style("fill-opacity", 1e-6);
    }

    public static appendDecCount(selection: any) {
        selection.append("text")
            .attr("y", (node: Node) => {
                return Node.getDecLabelHeight(node);
            })
            .attr("class", "decCount")
            .attr("dy", ".35em")
            .attr("text-anchor", "middle")
            .text((node: Node) => {
                if (Node.hasMoreChildren(node)) {
                    return node.decCount + " nodes below";
                }
            });
    }

    public static updateLocation(selection: any) {
        return selection.transition()
            .duration(Tree.duration)
            .attr("transform", (node: Node) => { return "translate(" + node.x + "," + node.y + ")"; });
    }

    public static updateCircle(selection: any) {
        // selection.transition()
            // .duration(Tree.duration)
            selection.select("circle")
            .attr("r", (node: Node) => {
                // console.log(node.id);
                let newRadius = Node.calculateRadius(node);
                // console.log(newRadius);
                return newRadius;
                // return Node.calculateRadius(node);
            })
            .each((d: Node) => { State.fillCircle(d); });
    }

    public static updateText(selection: any) {
        // selection.transition()
            // .duration(Tree.duration)
            selection.select("text")
            .style("fill-opacity", 1);

        // selection.transition()
            // .duration(Tree.duration)
            selection.select("text.decCount")
            .attr("y", (node: Node) => {
                return Node.getDecLabelHeight(node);
            })
            .text((node: Node) => {
                if (Node.hasMoreChildren(node)) {
                    return node.decCount + " Nodes below";
                }
            });
    }


    public static exitNodes(selection: any, source: Node) {
        return selection.exit().transition()
            .duration(Tree.duration)
            .attr("transform", (node: Node) => {
                // return "translate(" + source.x + "," + source.y + ")"; })
                let parent = node.parent;
                if (parent) {
                    return "translate(" + parent.x + "," + parent.y + ")";
                }
                return "translate(" + source.x + "," + source.y + ")";
            })
            .remove();
    }

    public static getLinkList(nodeList: Node[]) {
        return Tree.tree.links(nodeList);

    }

    public static enterLinks(linkDomObjects: any) {
        return linkDomObjects.enter().insert("path", "g")
            .attr("class", (link: any) => {
                if (State.solAncestorIds.includes(link.target.id) && State.solNodIds.length > 0) {
                    return "link";
                }
                return "link red";
            })
            .attr("d", (d: any) => {
                let o = { x: d.source.x, y: d.source.y };
                return Tree.diagonal({ source: o, target: o });
            })
            .style("stroke-opacity", 1e-6);
    }

    public static update(source: Node) {

        let nodeList = Tree.getNodeList(State.id2Node[State.rootId]);
        let linkList = Tree.getLinkList(nodeList);

        // let nodeList = Tree.tree.nodes(State.id2Node[State.rootId]),
        //     linkList = Tree.tree.links(nodeList);

        // nodeList.forEach((node: Node) => { node.y = node.depth * Tree.nodeHeight; });
        // console.log(nodeList);

        let nodeDomObjects = Tree.svg.selectAll("g.node")
            .data(nodeList, (node: Node) => { return node.id; });

        let nodeEnter = Tree.enterNodes(nodeDomObjects);

        // let nodeEnter = nodeDomObjects.enter().append("g")
        //     .attr("class", "node")
        //     .attr("id", (node: Node) => {
        //         return "node" + node.id;
        //     })
        //     .attr("transform", (node: Node) => {
        //         // console.log(node.parent);
        //         let parent = node.parent;
        //         if (parent) {
        //             return "translate(" + parent.x + "," + parent.y + ")";
        //         }
        //     })
        //     .on("click", (node: Node) => {
        //         Tree.selectNode(node.id);
        //     })
        //     .each((d: Node) => {
        //         if (!State.id2Node[d.id]) {
        //             State.id2Node[d.id] = d;
        //         }
        //     });

        Tree.appendCircle(nodeEnter);

        // nodeEnter.append("circle")
        //     .attr("r", (node: Node) => { return Node.calculateRadius(node); });


        // nodeEnter.append("text")
        //     .attr("y", () => {
        //         return -Tree.nodeHeight / 2;
        //     })
        //     .attr("dy", ".35em")
        //     .attr("text-anchor", "middle")
        //     .text((node: Node) => {

        //         if ($("#labels").prop("checked") === true) {
        //             return node.prettyLabel;
        //         }
        //         else {
        //             return node.label;
        //         }
        //     })
        //     .style("fill-opacity", 1e-6);

        Tree.appendLabel(nodeEnter);

        // nodeEnter.append("text")
        //     .attr("y", (node: Node) => {
        //         return Node.getDecLabelHeight(node);
        //     })
        //     .attr("class", "decCount")
        //     .attr("dy", ".35em")
        //     .attr("text-anchor", "middle")
        //     .text((node: Node) => {
        //         if (Node.hasMoreChildren(node)) {
        //             return node.decCount + " nodes below";
        //         }
        //     })

        Tree.appendDecCount(nodeEnter);


        // let nodeUpdate = nodeDomObjects.transition()
        //     .duration(Tree.duration)
        //     .attr("transform", (node: Node) => {
        //         return "translate(" + node.x + "," + node.y + ")";
        //     });

        let nodeUpdate = Tree.updateLocation(nodeDomObjects);

        // nodeUpdate.select("circle")
        //     .attr("r", (node: Node) => {
        //         return Node.calculateRadius(node);

        //     })
        //     .each((d: Node) => { State.fillCircle(d); });

        Tree.updateCircle(nodeUpdate);


        // nodeUpdate.select("text")
        //     .style("fill-opacity", 1);

        // nodeUpdate.select("text.decCount")
        //     .attr("y", (node: Node) => {
        //         return Node.getDecLabelHeight(node);
        //     })
        //     .text((node: Node) => {
        //         if (Node.hasMoreChildren(node)) {
        //             return node.decCount + " Nodes below";
        //         }
        //     });

        Tree.updateText(nodeUpdate);


        // let nodeExit = nodeDomObjects.exit().transition()
        //     .duration(Tree.duration)
        //     .attr("transform", (node: Node) => {
        //         // return "translate(" + source.x + "," + source.y + ")"; })
        //         let parent = node.parent;
        //         if (parent) {
        //             return "translate(" + parent.x + "," + parent.y + ")";
        //         }
        //         return "translate(" + source.x + "," + source.y + ")";
        //     })
        //     .remove();

        let nodeExit = Tree.exitNodes(nodeDomObjects, source);


        // nodeExit.select("circle")
        //     .attr("r", 1e-6);
        nodeExit.select("text")
            .style("fill-opacity", 1e-6);

        let linkDomObjects = Tree.svg.selectAll("path.link")
            .data(linkList, (link: any) => { return link.target.id; });



        // linkDomObjects.enter().insert("path", "g")
        //     .attr("class", (link: any) => {
        //         if (State.solAncestorIds.includes(link.target.id) && State.solNodIds.length > 0) {
        //             return "link";
        //         }
        //         return "link red";
        //     })
        //     .attr("d", (d: any) => {
        //         let o = { x: d.source.x, y: d.source.y };
        //         return Tree.diagonal({ source: o, target: o });
        //     })
        //     .style("stroke-opacity", 1e-6);

        Tree.enterLinks(linkDomObjects);

        linkDomObjects.transition()
            .duration(Tree.duration)
            .attr("d", Tree.diagonal)
            .style("stroke-opacity", 1);

        linkDomObjects.exit().transition()
            .duration(Tree.duration)
            .attr("d", (d: any) => {
                let o = { x: d.source.x, y: d.source.y };
                return Tree.diagonal({ source: o, target: o });
            })
            .remove();

        nodeList.forEach((node: Node) => {
            node.x0 = node.x;
            node.y0 = node.y;
        });
    }
}