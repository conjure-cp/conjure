import Mousetrap from "./util/mousetrap"
import { appendControls } from "./util/screen"
import globals from "./util/globals"
import colours from "./util/colours"
import * as listView from "./util/listView"

(function () {

    let init = true;

    window.addEventListener('message', event => {
        const message = event.data
        switch (message.command) {
            case 'loadChildren':
                globals.id2ChildIds[message.data.nodeId] = message.data.children
                update(globals.id2Node[message.data.nodeId]);

                break;
            case 'loadCore':
                message.data.forEach((element) => {

                    globals.correctPath.push(element.nodeId);
                });

                for (let i = 0; i < message.data.length; i++) {

                    let element = message.data[i]

                    if (!globals.id2Node[element.nodeId]) {

                        globals.addNode(element.nodeId, element.parentId, element.label);
                        globals.id2ChildIds[element.nodeId] = element.children

                        element.children.forEach(kidId => {

                            if (!globals.correctPath.includes(kidId)) {

                                globals.addNode(kidId, element.nodeId, message.data[i + 1].label.replace("!", ""))
                                globals.vscode.postMessage({
                                    command: 'loadChildren',
                                    id: kidId,
                                });
                            }

                        })
                    }
                }

                globals.collapseNode(1);

                update(globals.id2Node[0]);
                globals.waiting = false;

                globals.selectNode(globals.selectedId);

                $("#total").text(globals.totalLoaded + "/" + globals.correctPath[globals.correctPath.length - 1]);

                break;

            case 'longestBranchingVariable':
                globals.tree.nodeSize([Number(message.data) * 13, 0])

                break;

            case 'loadNodes':

                message.data.forEach((element) => {

                    if (!globals.id2Node[element.nodeId]) {
                        globals.addNode(element.nodeId, element.parentId, element.label);
                        globals.id2ChildIds[element.nodeId] = element.children
                    }
                });

                update(globals.id2Node[0]);
                globals.waiting = false;

                globals.selectNode(globals.selectedId);

                $("#total").text(globals.totalLoaded + "/" + globals.correctPath[globals.correctPath.length - 1]);

                break;

            case 'simpleDomains':

                listView.setNodeId(globals.selectedId);
                globals.currentDomainId += Number($("#domCount").val());

                if (globals.selectedId === 1) {
                    globals.simpleDomainsAtRoot = message.data.vars;

                    if (init) {
                        init = false;
                        globals.waiting = false;
                        break;
                    }

                    $("#pane").empty();
                    globals.tabulate()
                    globals.appendRows(message.data.vars);
                }

                else {
                    message.data.vars.forEach(variable => {
                        $("#" + $.escapeSelector(variable.name)).removeClass("changed");

                        let li = $("#" + $.escapeSelector(variable.name) + " > :last-child");
                        li.text(variable.rng);

                        if (message.data.changedNames.includes(variable.name)) {
                            $("#" + $.escapeSelector(variable.name)).toggleClass("changed");

                        }
                    });
                }
                globals.waiting = false;

                break;

            case 'prettyDomains':

                listView.setNodeId(globals.selectedId);

                if (globals.selectedId == 1) {
                    listView.render(message.data, message.data);
                }
                else {

                    listView.setChangedExpressions(message.data.changedExpressions);
                    listView.updateNodes(message.data.vars);
                    listView.setChangedList(message.data.changed);
                    listView.setChanged()
                }
                globals.waiting = false;

                break;
        }
        globals.waiting = false;
    });


    let zoom = d3.behavior.zoom()
        .on("zoom", zoomed);

    let svg = d3.select("#tree")
        .append("svg")
        .call(zoom)
        .attr("width", globals.viewerWidth)
        .attr("height", globals.viewerHeight)
        .append("g")

    function zoomed() {
        svg.attr("transform", "translate(" + d3.event.translate + ")" + " scale(" + d3.event.scale + ")")
    }


    function update(source) {

        let nodes = globals.tree.nodes(globals.id2Node[1]).reverse(),
            links = globals.tree.links(nodes);

        nodes.forEach((d) => { d.y = d.depth * 100; });

        let node = svg.selectAll("g.node")
            .data(nodes, (d) => { return d.id || (d.id = ++i); });

        let nodeEnter = node.enter().append("g")
            .attr("class", "node")
            .attr("id", (d) => {
                return "node" + d.id;
            })
            .attr("transform", (d) => {
                let parent = globals.id2Parent[d.id]
                if (parent) {
                    return "translate(" + parent.x + "," + parent.y + ")";
                }
            })
            .on("click", (d) => {
                globals.selectNode(d.id);
            })

        nodeEnter.append("circle")
            .attr("r", 1e-6)

        nodeEnter.append("text")
            .attr("y", (d) => {
                return -50
            })
            .attr("dy", ".35em")
            .attr("text-anchor", "middle")
            .text((d) => { return d.name; })
            .style("fill-opacity", 1e-6);

        let nodeUpdate = node.transition()
            .duration(globals.duration)
            .attr("transform", (d) => { return "translate(" + d.x + "," + d.y + ")"; })

        nodeUpdate.select("circle")
            .attr("r", 10)
            .each((d) => {

                let s = "#node" + d.id + " circle";

                let domElement = d3.select(s);
                domElement.classed("hasOthers red", false);

                let childLength = 0;
                if (d.children) {
                    childLength = d.children.length;
                }

                if (globals.id2ChildIds[d.id]) {
                    if (childLength < globals.id2ChildIds[d.id].length) {

                        if (!globals.correctPath.includes(d.id)) {

                            domElement.classed("hasOthers red", true);

                        }
                        domElement.classed("hasOthers", true);
                    }
                }

            })

        nodeUpdate.select("text")
            .style("fill-opacity", 1);


        let nodeExit = node.exit().transition()
            .duration(globals.duration)
            .attr("transform", (d) => { return "translate(" + source.x + "," + source.y + ")"; })
            .remove();
        nodeExit.select("circle")
            .attr("r", 1e-6);
        nodeExit.select("text")
            .style("fill-opacity", 1e-6);

        let link = svg.selectAll("path.link")
            .data(links, (d) => { return d.target.id; });

        link.enter().insert("path", "g")
            .attr("class", (d) => {

                if (globals.correctPath.includes(d.target.id)) {
                    return "link"
                }

                return "link red";

            })
            .attr("d", (d) => {
                let o = { x: d.source.x, y: d.source.y };
                return globals.diagonal({ source: o, target: o });
            })
            .style("stroke-opacity", 1e-6);

        link.transition()
            .duration(globals.duration)
            .attr("d", globals.diagonal)
            .style("stroke-opacity", 1);

        link.exit().transition()
            .duration(globals.duration)
            .attr("d", (d) => {
                let o = { x: d.source.x, y: d.source.y };
                return globals.diagonal({ source: o, target: o });
            })
            .remove();

        nodes.forEach((d) => {
            d.x0 = d.x;
            d.y0 = d.y;
        });

    }

    globals.setup(zoom);
    appendControls();
    Mousetrap.bind('s', () => {
        globals.nextNode();
        update(globals.id2Node[globals.selectedId]);
    }, 'keydown');
    Mousetrap.bind('w', globals.upNode, 'keydown');

    Mousetrap.bind('shift', globals.previousNode, 'keydown');

    Mousetrap.bind('d', globals.rightNode, 'keydown');
    Mousetrap.bind('a', () => {
        if (globals.id2Node[globals.selectedId].children) {
            if (globals.id2Node[globals.selectedId].children.length > 1) {
                globals.nextNode();
            }
        }
    }, 'keydown');

    Mousetrap.bind('t', () => {
        globals.toggleNode(globals.selectedId);
        update(globals.id2Node[globals.selectedId]);
    }, 'keydown');

    Mousetrap.bind('c', () => {
        globals.collapseNode(globals.selectedId);
        update(globals.id2Node[globals.selectedId]);
        globals.selectNode(globals.selectedId)
    }, 'keydown');

    Mousetrap.bind('e', () => {
        globals.expandNode(globals.selectedId);
        update(globals.id2Node[globals.selectedId]);
        globals.selectNode(globals.selectedId)
    }, 'keydown');

    Mousetrap.bind('m', () => {
        globals.loadDomains(globals.selectedId);
    }, 'keydown');

    Mousetrap.bind('f', () => {
        globals.collapseFailed();
        update(globals.id2Node[globals.selectedId]);
        globals.selectNode(globals.selectedId)
        // update(globals.id2Node[1]);
    }, 'keydown');

    // globals.loadNNodes();
    // globals.selectNode(globals.selectedId);
    console.log("HELLO")
    $("#form").validate();
})()