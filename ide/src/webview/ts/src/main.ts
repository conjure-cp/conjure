// let Mousetrap = require( './util/mousetrap');
// import { appendControls } from "./util/screen"
// import Globals, { loadNNodes } from "./util/Globals"
// import colours from "./util/colours"
// import * as listView from "./util/listView"



declare var d3: any;
declare var Mousetrap: any;
import Globals from './util/Globals';
import Listview from './util/Listview';

(function () {

    let init = true;

    window.addEventListener('message', event => {
        const message = event.data
        switch (message.command) {
            case 'loadChildren':
                Globals.id2ChildIds[message.data.nodeId] = message.data.children
                update(Globals.id2Node[message.data.nodeId]);

                break;
            case 'loadCore':

                console.log(message.data);

                message.data.forEach((element: any) => {
                    Globals.correctPath.push(element.nodeId);
                });

                for (let i = 0; i < message.data.length; i++) {

                    let element = message.data[i]

                    if (!Globals.id2Node[element.nodeId]) {

                        Globals.addNode(element.nodeId, element.parentId, element.label);
                        Globals.id2ChildIds[element.nodeId] = element.children

                        element.children.forEach((kidId: any) => {

                            if (!Globals.correctPath.includes(kidId)) {

                                Globals.addNode(kidId, element.nodeId, message.data[i + 1].label.replace("!", ""))
                                Globals.vscode.postMessage({
                                    command: 'loadChildren',
                                    id: kidId,
                                });
                            }

                        })
                    }
                }

                Globals.collapseNode(Globals.rootId);

                // console.log(Globals.id2Node);

                update(Globals.id2Node[Globals.rootId]);
                Globals.waiting = false;

                Globals.selectNode(Globals.selectedId);

                $("#total").text(Globals.totalLoaded + "/" + Globals.correctPath[Globals.correctPath.length - 1]);

                break;

            case 'longestBranchingVariable':
                Globals.tree.nodeSize([Number(message.data) * 13, 0])

                break;

            case 'loadNodes':

                message.data.forEach((element: any) => {

                    if (!Globals.id2Node[element.nodeId]) {
                        Globals.addNode(element.nodeId, element.parentId, element.label);
                        Globals.id2ChildIds[element.nodeId] = element.children
                    }
                });

                update(Globals.id2Node[0]);
                Globals.waiting = false;

                Globals.selectNode(Globals.selectedId);

                $("#total").text(Globals.totalLoaded + "/" + Globals.correctPath[Globals.correctPath.length - 1]);

                break;

            case 'simpleDomains':

                Listview.setNodeId();
                Globals.currentDomainId += Number($("#domCount").val());

                if (Globals.selectedId === Globals.rootId) {
                    Globals.simpleDomainsAtRoot = message.data.vars;

                    if (init) {
                        init = false;
                        Globals.waiting = false;
                        break;
                    }

                    $("#pane").empty();
                    Globals.tabulate()
                    Globals.appendRows(message.data.vars);
                }

                else {
                    message.data.vars.forEach((variable: any) => {
                        $("#" + $.escapeSelector(variable.name)).removeClass("changed");

                        let li = $("#" + $.escapeSelector(variable.name) + " > :last-child");
                        li.text(variable.rng);

                        if (message.data.changedNames.includes(variable.name)) {
                            $("#" + $.escapeSelector(variable.name)).toggleClass("changed");

                        }
                    });
                }
                Globals.waiting = false;

                break;

            case 'prettyDomains':

                Listview.setNodeId();

                if (Globals.selectedId == Globals.rootId) {
                    Listview.render(message.data, message.data);
                }
                else {
                    Listview.setChangedExpressions(message.data.changedExpressions);
                    Listview.updateNodes(message.data.vars);
                    Listview.setChangedList(message.data.changed);
                    Listview.setChanged()
                }
                Globals.waiting = false;
                break;
        }
        Globals.waiting = false;
    });


    let zoom = d3.behavior.zoom()
        .on("zoom", zoomed);

    let svg = d3.select("#tree")
        .append("svg")
        .call(zoom)
        .attr("width", Globals.viewerWidth)
        .attr("height", Globals.viewerHeight)
        .append("g")

    function zoomed() {
        svg.attr("transform", "translate(" + d3.event.translate + ")" + " scale(" + d3.event.scale + ")")
    }


    function update(source: any) {

        let nodes = Globals.tree.nodes(Globals.id2Node[Globals.rootId]).reverse(),
            links = Globals.tree.links(nodes);

        nodes.forEach((d: any) => { d.y = d.depth * 100; });

        let node = svg.selectAll("g.node")
            .data(nodes, (d: any) => { return d.id; });

        let nodeEnter = node.enter().append("g")
            .attr("class", "node")
            .attr("id", (d: any) => {
                return "node" + d.id;
            })
            .attr("transform", (d: any) => {
                let parent = Globals.id2Node[d.id].parent;
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

                if (Globals.id2ChildIds[d.id]) {
                    if (childLength < Globals.id2ChildIds[d.id].length) {

                        if (!Globals.correctPath.includes(d.id)) {

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

        let link = svg.selectAll("path.link")
            .data(links, (d: any) => { return d.target.id; });

        link.enter().insert("path", "g")
            .attr("class", (d: any) => {

                if (Globals.correctPath.includes(d.target.id)) {
                    return "link"
                }

                return "link red";

            })
            .attr("d", (d: any) => {
                let o = { x: d.source.x, y: d.source.y };
                return Globals.diagonal({ source: o, target: o });
            })
            .style("stroke-opacity", 1e-6);

        link.transition()
            .duration(Globals.duration)
            .attr("d", Globals.diagonal)
            .style("stroke-opacity", 1);

        link.exit().transition()
            .duration(Globals.duration)
            .attr("d", (d: any) => {
                let o = { x: d.source.x, y: d.source.y };
                return Globals.diagonal({ source: o, target: o });
            })
            .remove();

        nodes.forEach((d: any) => {
            d.x0 = d.x;
            d.y0 = d.y;
        });

    }

    Globals.setup(zoom);
    // appendControls();
    Mousetrap.bind('s', () => {
        Globals.nextNode();
        update(Globals.id2Node[Globals.selectedId]);
    }, 'keydown');
    Mousetrap.bind('w', Globals.upNode, 'keydown');

    Mousetrap.bind('shift', Globals.previousNode, 'keydown');

    Mousetrap.bind('d', Globals.rightNode, 'keydown');
    Mousetrap.bind('a', () => {
        if (Globals.id2Node[Globals.selectedId].children) {
            if (Globals.id2Node[Globals.selectedId].children.length > 1) {
                Globals.nextNode();
            }
        }
    }, 'keydown');

    Mousetrap.bind('t', () => {
        Globals.toggleNode(Globals.selectedId);
        update(Globals.id2Node[Globals.selectedId]);
    }, 'keydown');

    Mousetrap.bind('c', () => {
        Globals.collapseNode(Globals.selectedId);
        update(Globals.id2Node[Globals.selectedId]);
        Globals.selectNode(Globals.selectedId)
    }, 'keydown');

    Mousetrap.bind('e', () => {
        Globals.expandNode(Globals.selectedId);
        update(Globals.id2Node[Globals.selectedId]);
        Globals.selectNode(Globals.selectedId)
    }, 'keydown');

    Mousetrap.bind('m', () => {
        Globals.loadDomains();
    }, 'keydown');

    Mousetrap.bind('f', () => {
        Globals.collapseFailed();
        update(Globals.id2Node[Globals.selectedId]);
        Globals.selectNode(Globals.selectedId)
        // update(Globals.id2Node[1]);
    }, 'keydown');

    Globals.loadNNodes();
    Globals.selectNode(Globals.selectedId);
    console.log("HELLO");
    console.log(Globals.id2Node);
    // $("#form").validate();
})()