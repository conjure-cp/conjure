import Node from './Node';
import Data from './Data';
import Tree from './Tree';

declare var acquireVsCodeApi: any;
declare var d3: any;

export default class Globals {
    public static data = new Data();
    public static vscode = acquireVsCodeApi();
    public static duration = 750;

    public static nextNode = () => {

        let stepSize = Number($("#stepSize").val());

        if (Globals.data.id2Node[Globals.data.selectedId]._children) {
            // console.log("NOW")
            Globals.data.toggleNode(Globals.data.selectedId);
            return
        }

        if (!Globals.data.id2Node[Globals.data.selectedId + stepSize]) {
            Globals.loadNNodes();
            // console.log("NEXT")
        }
        else {
            // exports.selectedId = exports.currentId;
            console.log("here!!!")
            Globals.data.selectedId += stepSize;
            Globals.selectNode(Globals.data.selectedId);
        }
    }

    //TODO check that the previous node is not withn a collapsed node
    public static previousNode = () => {
        if (Globals.data.selectedId > 1) {
            Globals.data.selectedId--;
            Globals.selectNode(Globals.data.selectedId);
        }
    }

    public static rightNode = () => {
        if (Globals.data.id2Node[Globals.data.selectedId].children) {
            let childCount = Globals.data.id2Node[Globals.data.selectedId].children.length;
            if (childCount > 1) {
                Globals.data.selectedId = Globals.data.id2Node[Globals.data.selectedId].children[childCount - 1].id;
            }
            Globals.selectNode(Globals.data.selectedId);
        }
    }

    public static upNode = () => {
        if (Globals.data.selectedId > Globals.data.rootId) {
            // exports.selectedId = exports.id2Parent[exports.selectedId].id;
            Globals.data.selectedId = Globals.data.id2Node[Globals.data.selectedId].parent.id;
        }
        Globals.selectNode(Globals.data.selectedId);
    }

    public static loadNNodes = () => {

        if (!Globals.data.waiting) {

            // console.log("requesting more");

            Globals.vscode.postMessage({
                command: 'loadNodes',
                amount: Number($("#stepSize").val()),
                start: Globals.data.selectedId
                // start: exports.currentId
            });

            // console.log("SET WAIT TRUE NODES");
            Globals.data.waiting = true;
        }
        else {
            // console.log("waiting");

        }
    }

    public static selectNode = (nodeId: number) => {
        Globals.data.selectedId = nodeId;

        let allCircles = ".node circle"
        d3.selectAll(allCircles).classed("selected", false);
        let s = "#node" + nodeId + " circle";
        d3.select(s).classed("selected", true);

        // console.log(nodeId)
        // console.log(exports.id2Node[nodeId])

        Tree.focusNode(Globals.data.id2Node[nodeId]);


        Globals.data.currentDomainId = 0;

        // console.log("Calling load domains");
        // if (!exports.pretty) {
        //     $("#pane").empty();
        //     exports.tabulate()
        // }

        if (!Globals.data.frozen) {
            Globals.loadDomains();
        }

    }

    public static loadDomains = () => {

        // console.log(exports.waiting);

        if (!Globals.data.waiting) {

            if (Globals.data.pretty) {
                Globals.sendPrettyRequest()
            }
            else {
                Globals.sendSimpleRequest()
            }

            Globals.data.waiting = true;
        }
        // console.log("SET WAIT TRUE DOMAINS, pretty: " + exports.pretty );
    }
    public static sendSimpleRequest = () => {
        Globals.vscode.postMessage({
            command: "simpleDomains",
            amount: Number($("#domCount").val()),
            start: Globals.data.currentDomainId,
            nodeId: Globals.data.selectedId,
        });
    }

    public static sendPrettyRequest = () => {
        Globals.vscode.postMessage({
            command: "prettyDomains",
            nodeId: Globals.data.selectedId,
            paths: Globals.data.pathList.join(":")
        });
    }



    public static tabulate = () => {
        var table = d3.select('#pane').append('table')
        var thead = table.append('thead')

        // append the header row
        thead.append('tr')
            .selectAll('th')
            .data(Globals.data.columns).enter()
            .append('th')
            .text(function (column: any) { return column; });
    }

    public static appendRows = (data: any) => {
        var table = d3.select('#pane').append('table');
        var tbody = table.append('tbody');

        var rows = tbody.selectAll('tr')
            .data(data)
            .enter()
            .append('tr')
            .attr("id", (d: any, i: any) => {
                // console.log(d);
                // return "row" + (i + exports.currentDomainId - Number($("#domCount").val())) 
                return d.name;
            })

        // create a cell in each row for each column
        var cells = rows.selectAll('td')
            .data((row: any) => {
                return Globals.data.columns.map((column) => {
                    //dasdas
                    ////dasdas
                    return { column: column, value: row[column] };
                });
            })
            .enter()
            .append('td')
            .text((d: any) => { return d.value; });
    }

    public static getChildren = (parentId: number) => {
        Globals.vscode.postMessage({
            command: 'children',
            parentId: parentId,
        });
    }


    // public static zoom = d3.behavior.zoom()
    //     .on("zoom", Globals.zoomed);

    // public static svg = d3.select("#tree")
    //     .append("svg")
    //     .call(Globals.zoom)
    //     .attr("width", Globals.viewerWidth)
    //     .attr("height", Globals.viewerHeight)
    //     .append("g")

    // public static zoomed() {
    //     Globals.svg.attr("transform", "translate(" + d3.event.translate + ")" + " scale(" + d3.event.scale + ")")
    // }

    // public static update(source: any) {

    //     console.log("update called!");

    //     let nodes = Globals.tree.nodes(Globals.data.id2Node[Globals.data.rootId]).reverse(),
    //         links = Globals.tree.links(nodes);

    //     nodes.forEach((d: any) => { d.y = d.depth * 100; });

    //     let node = Globals.svg.selectAll("g.node")
    //         .data(nodes, (d: any) => { return d.id; });

    //     let nodeEnter = node.enter().append("g")
    //         .attr("class", "node")
    //         .attr("id", (d: any) => {
    //             return "node" + d.id;
    //         })
    //         .attr("transform", (d: any) => {
    //             let parent = Globals.data.id2Node[d.id].parent;
    //             if (parent) {
    //                 return "translate(" + parent.x + "," + parent.y + ")";
    //             }
    //         })
    //         .on("click", (d: any) => {
    //             Globals.selectNode(d.id);
    //         })

    //     nodeEnter.append("circle")
    //         .attr("r", 1e-6)

    //     nodeEnter.append("text")
    //         .attr("y", () => {
    //             return -50
    //         })
    //         .attr("dy", ".35em")
    //         .attr("text-anchor", "middle")
    //         .text((d: any) => { return d.name; })
    //         .style("fill-opacity", 1e-6);

    //     let nodeUpdate = node.transition()
    //         .duration(Globals.duration)
    //         .attr("transform", (d: any) => { return "translate(" + d.x + "," + d.y + ")"; })

    //     nodeUpdate.select("circle")
    //         .attr("r", 10)
    //         .each((d: any) => {

    //             let s = "#node" + d.id + " circle";

    //             let domElement = d3.select(s);
    //             domElement.classed("hasOthers red", false);

    //             let childLength = 0;
    //             if (d.children) {
    //                 childLength = d.children.length;
    //             }

    //             if (Globals.data.id2ChildIds[d.id]) {
    //                 if (childLength < Globals.data.id2ChildIds[d.id].length) {

    //                     if (!Globals.data.correctPath.includes(d.id)) {

    //                         domElement.classed("hasOthers red", true);

    //                     }
    //                     domElement.classed("hasOthers", true);
    //                 }
    //             }

    //         })

    //     nodeUpdate.select("text")
    //         .style("fill-opacity", 1);


    //     let nodeExit = node.exit().transition()
    //         .duration(Globals.duration)
    //         .attr("transform", (d: any) => { return "translate(" + source.x + "," + source.y + ")"; })
    //         .remove();
    //     nodeExit.select("circle")
    //         .attr("r", 1e-6);
    //     nodeExit.select("text")
    //         .style("fill-opacity", 1e-6);

    //     let link = Globals.svg.selectAll("path.link")
    //         .data(links, (d: any) => { return d.target.id; });

    //     link.enter().insert("path", "g")
    //         .attr("class", (d: any) => {

    //             if (Globals.data.correctPath.includes(d.target.id)) {
    //                 return "link"
    //             }

    //             return "link red";

    //         })
    //         .attr("d", (d: any) => {
    //             let o = { x: d.source.x, y: d.source.y };
    //             return Globals.diagonal({ source: o, target: o });
    //         })
    //         .style("stroke-opacity", 1e-6);

    //     link.transition()
    //         .duration(Globals.duration)
    //         .attr("d", Globals.diagonal)
    //         .style("stroke-opacity", 1);

    //     link.exit().transition()
    //         .duration(Globals.duration)
    //         .attr("d", (d: any) => {
    //             let o = { x: d.source.x, y: d.source.y };
    //             return Globals.diagonal({ source: o, target: o });
    //         })
    //         .remove();

    //     nodes.forEach((d: any) => {
    //         d.x0 = d.x;
    //         d.y0 = d.y;
    //     });

    // }


    public static initialize = () => {
        Globals.vscode.postMessage({
            command: 'init',
        });


        Globals.vscode.postMessage({
            command: 'correctPath',
        });

        Globals.vscode.postMessage({
            command: 'longestBranchingVariable',
        });

        Globals.vscode.postMessage({
            command: 'loadCore',
        });

        Globals.vscode.postMessage({
            command: 'simpleDomains',
            amount: Number($("#domCount").val()),
            start: 0,
            nodeId: Globals.data.rootId,
        });
    }
}

Globals.initialize();



// exports.id2Parent = {};











