import Mousetrap from "./util/mousetrap"
import { appendControls } from "./util/screen"
import globals from "./util/globals"
import showPretty from "./util/panel"
import colours from "./util/colours"
import * as listView from "./util/listView"

(function () {

    window.addEventListener('message', event => {
        const message = event.data
        switch (message.command) {

            case 'loadNodes':


                var rootNode;

                message.data.forEach((element) => {

                    if (element.parentId === -1) {
                        rootNode = true;
                    }

                    globals.addNode(element.parentId);
                    globals.id2ChildIds[globals.currentId] = element.children

                });

                update(globals.id2Node[globals.currentId]);
                globals.waiting = false;

                if (rootNode) {
                    globals.selectNode(1);
                }
                break;
            case 'simpleDomains':
                globals.currentDomainId += Number($("#domCount").val());
                // console.log(message.data);
                // console.log(globals.currentDomainId);
                globals.appendRows(message.data.vars);
                message.data.changedIds.forEach(rowId => {

                    let accountForExtra = globals.currentDomainId - Number($("#domCount").val()) + Number(rowId)

                    $("#row" + accountForExtra).toggleClass("changed");

                });
                break;

            case 'prettyDomains':
                // console.log(message.data);
                // console.log(globals.selectedId);

                if (globals.selectedId == 1) {
                    listView.render(message.data, message.data);
                }
                else {
                    listView.updateNodes(message.data.vars);
                    listView.setChangedList(message.data.changed);
                    listView.setChanged()
                }

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
        // Compute the new tree layout.
        let nodes = globals.tree.nodes(globals.id2Node[1]).reverse(),
            links = globals.tree.links(nodes);

        // Normalize for fixed-depth.
        nodes.forEach((d) => { d.y = d.depth * 100; });
        // Declare the nodes…
        let node = svg.selectAll("g.node")
            .data(nodes, (d) => { return d.id || (d.id = ++i); });
        // Enter the nodes.
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
        // .on("mouseover", showDomains);

        nodeEnter.append("circle")
            .transition()
            .attr("r", 10)


        nodeEnter.append("text")
            .attr("y", (d) => {
                return d.children || d._children ? -18 : 18;
            })
            .attr("dy", ".35em")
            .attr("text-anchor", "middle")
            .text((d) => { return d.name; })
            .style("fill-opacity", 1e-6);
        // Transition nodes to their new position.
        //horizontal tree
        let nodeUpdate = node.transition()
            .duration(globals.duration)
            .attr("transform", (d) => { return "translate(" + d.x + "," + d.y + ")"; })

        nodeUpdate.select("circle")
            .attr("r", 10)
            .each((d) => {

                let childLength = 0;
                if (d.children) {
                    childLength = d.children.length;
                }

                // console.log(d)
                // console.log(d.id);
                //  console.log(globals.id2ChildIds);
                // console.log(childLength);

                if (globals.id2ChildIds[d.id]) {
                    if (childLength < globals.id2ChildIds[d.id].length) {
                        globals.setHasOthers(d.id);
                    }
                    else {
                        globals.unsetHasOthers(d.id);
                    }
                }
            });

        // .style("fill", (d) => { return d._children ? "#6ac4a1" : "#fff"; });
        nodeUpdate.select("text")
            .style("fill-opacity", 1);

        // console.log(nodeUpdate.select("g"));

        // Transition exiting nodes to the parent's new position.
        let nodeExit = node.exit().transition()
            .duration(globals.duration)
            .attr("transform", (d) => { return "translate(" + source.x + "," + source.y + ")"; })
            .remove();
        nodeExit.select("circle")
            .attr("r", 1e-6);
        nodeExit.select("text")
            .style("fill-opacity", 1e-6);
        // Update the links…
        // Declare the links…
        let link = svg.selectAll("path.link")
            .data(links, (d) => { return d.target.id; });
        // Enter the links.
        link.enter().insert("path", "g")
            .attr("class", (d) => {

                let node = d.source

                let start = "link"
                let parent = globals.id2Parent[node.id];

                if (parent) {
                    let kids = globals.id2ChildIds[parent.id];

                    if (kids[kids.length - 1] != node.id) {
                        console.log(node.id);
                        console.log(kids[kids.length - 1]);
                        start += " red"
                    }

                }

                return start;

            })
            .attr("d", (d) => {
                let o = { x: d.source.x, y: d.source.y };
                return globals.diagonal({ source: o, target: o });
            });
        // Transition links to their new position.
        link.transition()
            .duration(globals.duration)
            .attr("d", globals.diagonal);
        // Transition exiting nodes to the parent's new position.
        link.exit().transition()
            .duration(globals.duration)
            .attr("d", (d) => {
                let o = { x: d.source.x, y: d.source.y };
                return globals.diagonal({ source: o, target: o });
            })
            .remove();

        // Stash the old positions for transition.
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
    }, 'keydown');

    Mousetrap.bind('e', () => {
        globals.expandNode(globals.selectedId);
        update(globals.id2Node[globals.selectedId]);
    }, 'keydown');

    Mousetrap.bind('m', () => {
        globals.loadDomains(globals.selectedId);
    }, 'keydown');

    globals.loadNNodes();
    console.log("HELLO")
})()