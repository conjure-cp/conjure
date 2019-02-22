declare var d3: any;
import Globals from './Globals';

export default class Tree {

    public static viewerWidth = $(document).width();
    public static viewerHeight = $(document).height();
    public static margin = { top: 40, right: 30, bottom: 50, left: 30 };
    public static width = Tree.viewerWidth! - Tree.margin.left - Tree.margin.right;
    public static height = Tree.viewerHeight! - Tree.margin.top - Tree.margin.bottom;

    public static zoom = d3.behavior.zoom()
        .on("zoom", Tree.zoomed);

    public static svg = d3.select("#tree")
        .append("svg")
        .call(Tree.zoom)
        .attr("width", Tree.viewerWidth)
        .attr("height", Tree.viewerHeight)
        .append("g")

    public static tree = d3.layout.tree().size([Tree.height, Tree.width]);

    public static diagonal = d3.svg.diagonal()
        .projection((d: any) => {
            return [d.x, d.y];
        });

    public static zoomed() {
        Tree.svg.attr("transform", "translate(" + d3.event.translate + ")" + " scale(" + d3.event.scale + ")")
    }

    public static focusNode = (node: any) => {
        // scale = 7;''
        // console.log(node);
        let scale = Tree.zoom.scale();
        let x = -node.x * scale;
        let y = -node.y * scale;

        x += Tree.width / 3;
        y += Tree.height / 2;

        d3.select('g').transition()
            .duration(Globals.duration)
            .attr("transform", "translate(" + x + "," + y + ")scale(" + scale + ")");
       Tree.zoom.translate([x, y]);
    }

    public static update(source: any) {

        let nodes = Tree.tree.nodes(Globals.data.id2Node[Globals.data.rootId]).reverse(),
            links = Tree.tree.links(nodes);

        nodes.forEach((d: any) => { d.y = d.depth * 100; });

        let node = Tree.svg.selectAll("g.node")
            .data(nodes, (d: any) => { return d.id; });

        let nodeEnter = node.enter().append("g")
            .attr("class", "node")
            .attr("id", (d: any) => {
                return "node" + d.id;
            })
            .attr("transform", (d: any) => {
                let parent = Globals.data.id2Node[d.id].parent;
                if (parent) {
                    return "translate(" + parent.x + "," + parent.y + ")";
                }
            })
            .on("click", (d: any) => {
                Globals.selectNode(d.id);
            })

        nodeEnter.append("circle")
            .attr("r", 1e-6)

        nodeEnter.append("text")
            .attr("y", () => {
                return -50
            })
            .attr("dy", ".35em")
            .attr("text-anchor", "middle")
            .text((d: any) => { return d.name; })
            .style("fill-opacity", 1e-6);

        let nodeUpdate = node.transition()
            .duration(Globals.duration)
            .attr("transform", (d: any) => { return "translate(" + d.x + "," + d.y + ")"; })

        nodeUpdate.select("circle")
            .attr("r", 10)
            .each((d: any) => {

                let s = "#node" + d.id + " circle";

                let domElement = d3.select(s);
                domElement.classed("hasOthers red", false);

                let childLength = 0;
                if (d.children) {
                    childLength = d.children.length;
                }

                if (Globals.data.id2ChildIds[d.id]) {
                    if (childLength < Globals.data.id2ChildIds[d.id].length) {

                        if (!Globals.data.correctPath.includes(d.id)) {

                            domElement.classed("hasOthers red", true);

                        }
                        domElement.classed("hasOthers", true);
                    }
                }

            })

        nodeUpdate.select("text")
            .style("fill-opacity", 1);


        let nodeExit = node.exit().transition()
            .duration(Globals.duration)
            .attr("transform", (d: any) => { return "translate(" + source.x + "," + source.y + ")"; })
            .remove();
        nodeExit.select("circle")
            .attr("r", 1e-6);
        nodeExit.select("text")
            .style("fill-opacity", 1e-6);

        let link = Tree.svg.selectAll("path.link")
            .data(links, (d: any) => { return d.target.id; });

        link.enter().insert("path", "g")
            .attr("class", (d: any) => {

                if (Globals.data.correctPath.includes(d.target.id)) {
                    return "link"
                }

                return "link red";

            })
            .attr("d", (d: any) => {
                let o = { x: d.source.x, y: d.source.y };
                return Tree.diagonal({ source: o, target: o });
            })
            .style("stroke-opacity", 1e-6);

        link.transition()
            .duration(Globals.duration)
            .attr("d", Tree.diagonal)
            .style("stroke-opacity", 1);

        link.exit().transition()
            .duration(Globals.duration)
            .attr("d", (d: any) => {
                let o = { x: d.source.x, y: d.source.y };
                return Tree.diagonal({ source: o, target: o });
            })
            .remove();

        nodes.forEach((d: any) => {
            d.x0 = d.x;
            d.y0 = d.y;
        });

    }
}
