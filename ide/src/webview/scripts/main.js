import Mousetrap from "./util/mousetrap"
import { appendControls } from "./util/screen"
import globals from "./util/globals"
import panel from "./util/panel"

(function () {

    window.addEventListener('message', event => {
        const message = event.data
        switch (message.command) {

            case 'loadNodes':

                // console.log(message.data);

                var rootNode;

                message.data.forEach((element) => {

                    if (element.parentId === -1){
                        rootNode = true;
                    }

                    globals.addNode(element.parentId);
                    globals.id2ChildIds[globals.currentId] = element.children

                });

                update(globals.id2Node[globals.currentId]);
                globals.waiting = false;

                if (rootNode){
                    globals.selectNode(1);
                }
                break;
        }
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
                    else{
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
            .attr("class", "link")
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
    Mousetrap.bind('w', globals.previousNode, 'keydown');
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


    globals.loadNNodes();
    d3.select("h1").text(function (d) { return "HELLO"; });
    console.log("HELLO")
})()


// console.log(dasdasdA)

// let panel = require('./util/panel.js');
// let colours = require('./util/colours.js');

// (function () {

//     const vscode = acquireVsCodeApi();

//     vscode.postMessage({
//         command: 'ready',
//     });

//     window.addEventListener('message', event => {

//         $("#tree").empty();
//         $("#controls").empty();

//         const root = event.data.tree; // The JSON data our extension sent
//         const treeviewDomainMap = event.data.treeviewDomainMap; // The JSON data our extension sent
//         const normalDomainMap = event.data.normalDomainMap; // The JSON data our extension sent
//         const simpleDomainMap = event.data.simpleDomainMap; // The JSON data our extension sent

//         let parentMap = {};
//         let globals.id2Node = {};
//         let selectedNode;
//         let selectedCircle;
//         let init = true;
//         let allTreeViewNodes;

//         Mousetrap.bind('n', next, 'keydown');
//         Mousetrap.bind('p', previous, 'keydown');
//         Mousetrap.bind('r', goRight, 'keydown');
//         Mousetrap.bind('l', goLeft, 'keydown');
//         Mousetrap.bind('u', goUp, 'keydown');
//         Mousetrap.bind('t', () => { nodeToggle(globals.id2Node[selectedNode]) }, 'keydown');
//         Mousetrap.bind('e', expander);
//         Mousetrap.bind('c', collapser);
//         Mousetrap.bind('r', () => { vscode.postMessage({ command: 'ready', }) });


//         function goUp() {

//             let nextNode;

//             if (selectedNode in parentMap) {
//                 nextNode = parentMap[selectedNode];
//             }
//             else {
//                 for (const key in globals.id2Node) {
//                     if (globals.id2Node[key].children) {
//                         if (globals.id2Node[key].children.includes(globals.id2Node[selectedNode])) {
//                             nextNode = globals.id2Node[key];
//                             break;
//                         }
//                     }
//                 }
//             }

//             parentMap[selectedNode] = nextNode;
//             selectedNode = nextNode.minionID;

//             selectNode(selectedNode);
//             focusNode(globals.id2Node[selectedNode]);
//         }

//         function goLeft() {
//             if (globals.id2Node[selectedNode].children.length > 0) {
//                 selectedNode = globals.id2Node[selectedNode].children[0].minionID;
//                 selectNode(selectedNode);
//                 focusNode(globals.id2Node[selectedNode]);
//             }
//         }

//         function goRight() {
//             if (globals.id2Node[selectedNode].children.length == 2) {
//                 selectedNode = globals.id2Node[selectedNode].children[1].minionID;
//                 selectNode(selectedNode);
//                 focusNode(globals.id2Node[selectedNode]);
//             }
//         }

//         function validDest(minionID) {
//             // console.log(d3.select("#node" + minionID));
//             if (d3.select("#node" + minionID)[0][0]) {
//                 if (minionID > 0) {
//                     return true;
//                 }
//             }
//             return false;
//         }

//         function next() {
//             let temp = (selectedNode + 1) % (Object.keys(treeviewDomainMap).length + 1);
//             if (validDest(temp)) {
//                 selectedNode = temp;
//                 selectNode(selectedNode);
//                 focusNode(globals.id2Node[selectedNode]);
//             }
//             else {
//                 if (globals.id2Node[selectedNode]._children) {
//                     globals.id2Node[selectedNode].children = globals.id2Node[selectedNode]._children;
//                     globals.id2Node[selectedNode]._children = null;
//                     update(globals.id2Node[selectedNode]);
//                     next();
//                 }
//             }
//         }

//         function previous() {
//             let temp = (selectedNode - 1) % (Object.keys(treeviewDomainMap).length + 1);
//             if (validDest(temp)) {
//                 selectedNode = temp;
//                 selectNode(selectedNode);
//                 focusNode(globals.id2Node[selectedNode]);
//             }
//         }

//         // Add controls

//         d3.select("#controls")
//             .append("input")
//             .attr("type", "button")
//             .attr("value", "Collapse All")
//             .on("click", collapser);

//         d3.select("#controls")
//             .append("input")
//             .attr("type", "button")
//             .attr("value", "Expand All")
//             .on("click", expander);

//         d3.select("#controls")
//             .append("input")
//             .attr("type", "button")
//             .attr("value", "Find Root")
//             .on("click", () => {
//                 selectNode(root.minionID);
//                 focusNode(globals.id2Node[root.minionID]);
//             });

//         d3.select("#controls")
//             .append("input")
//             .attr("type", "button")
//             .attr("value", "Previous")
//             .on("click", () => {
//                 previous();
//             });

//         d3.select("#controls")
//             .append("input")
//             .attr("type", "button")
//             .attr("value", "Next")
//             .on("click", () => {
//                 next();
//             });

//         d3.select("#controls")
//             .append("input")
//             .attr("type", "button")
//             .attr("value", "Toggle")
//             .on("click", () => {
//                 nodeToggle(globals.id2Node[selectedNode]);
//             });

//         d3.select("#controls")
//             .append('label')
//             .text("Pretty")
//             .append("input")
//             .attr("checked", true)
//             .attr("type", "checkbox")
//             .attr("id", "check")
//             .on("change", () => {
//                 // console.log("Changed!")
//                 showDomains(selectedNode)
//             })
//         // .attr("onClick", () => {
//         //     console.log("hello");
//         //     showDomains(selectedNode)
//         // });

//         // set the dimensions and margins of the diagram
//         let viewerWidth = $(document).width();
//         let viewerHeight = $(document).height();
//         let margin = { top: 40, right: 30, bottom: 50, left: 30 },
//             width = viewerWidth - margin.left - margin.right,
//             height = viewerHeight - margin.top - margin.bottom;

//         let i = 0, duration = 750;
//         let tree = d3.layout.tree().size([height, width]);

//         let diagonal = d3.svg.diagonal()
//             .projection((d) => {
//                 return [d.x, d.y];

//             });

//         let zoom = d3.behavior.zoom()
//             .on("zoom", zoomed);

//         let svg = d3.select("#tree")
//             .append("svg")
//             .call(zoom)
//             .attr("width", viewerWidth)
//             .attr("height", viewerHeight)
//             .append("g")


//         var data = [
//             { name: 'left', orient: "134", refX: 20, refY: -4, colour: "red" },
//             { name: 'right', orient: "40", refX: 20, refY: 5.5, colour: "orange" },
//             { name: 'straight', orient: "auto", refX: 20, refY: 0, colour: "yellow" }
//         ]


//         svg.append("svg:defs").selectAll("marker")
//             .data(data)      // Different link/path types can be defined here
//             .enter()
//             .append("svg:marker")    // This section adds in the arrows
//             .attr('id', function (d) { return 'marker_' + d.name })
//             .attr("viewBox", "0 -5 10 10")
//             .attr("orient", (d) => { return d.orient })
//             .attr("refX", (d) => { return d.refX })
//             .attr("refY", (d) => { return d.refY })
//             .attr("markerWidth", 10)
//             .attr("markerHeight", 6)
//             .append("svg:path")
//             .attr("d", "M0, -5L10, 0L0, 5")
//             .style("fill", (d) => { return "#0dbc79" });

//         function zoomed() {
//             svg.attr("transform", "translate(" + d3.event.translate + ")" + " scale(" + d3.event.scale + ")")
//             // zoom.translate(d3.event.translate);

//         }

//         function focusNode(node) {
//             // scale = 7;
//             scale = zoom.scale();
//             let x = -node.x * scale;
//             let y = -node.y * scale;

//             x += width / 3;
//             y += height / 2;

//             d3.select('g').transition()
//                 .duration(duration)
//                 .attr("transform", "translate(" + x + "," + y + ")scale(" + scale + ")");
//             zoom.translate([x, y]);
//         }


//         function update(source) {
//             // Compute the new tree layout.
//             let nodes = tree.nodes(root).reverse(),
//                 links = tree.links(nodes);

//             // Normalize for fixed-depth.
//             nodes.forEach((d) => { d.y = d.depth * 100; });
//             // Declare the nodes…
//             let node = svg.selectAll("g.node")
//                 .data(nodes, (d) => { return d.id || (d.id = ++i); });
//             // Enter the nodes.
//             let nodeEnter = node.enter().append("g")
//                 .attr("class", "node")
//                 .attr("id", (d) => {
//                     return "node" + d.minionID;
//                 })
//                 .attr("transform", (d) => {
//                     globals.id2Node[Number(d.minionID)] = d;
//                     if (source.x0 && source.y0) {
//                         return "translate(" + source.x0 + "," + source.y0 + ")";
//                     }
//                 })
//                 .on("click", (d) => {
//                     selectNode(d.minionID);
//                 });
//             // .on("mouseover", showDomains);

//             nodeEnter.append("circle")
//                 .transition()
//                 .attr("r", 10)
//                 .attr("stroke", (d) => { return d.children || d._children ? "steelblue" : "#00c13f"; })
//                 .style("fill", (d) => { return d.children || d._children ? "#6ac4a1" : "#fff"; });

//             nodeEnter.append("text")
//                 .attr("y", (d) => {
//                     if (d.children || d._children) {
//                         return 42;
//                     }
//                     return 20;
//                 })
//                 .attr("dy", ".35em")
//                 .attr("text-anchor", "middle")
//                 .text((d) => { return d.name; })
//                 .style("fill-opacity", 1e-6);
//             // Transition nodes to their new position.
//             //horizontal tree
//             let nodeUpdate = node.transition()
//                 .duration(duration)
//                 .attr("transform", (d) => { return "translate(" + d.x + "," + d.y + ")"; });
//             nodeUpdate.select("circle")
//                 .attr("r", 10)
//                 .style("fill", (d) => { return d._children ? "#6ac4a1" : "#fff"; });
//             nodeUpdate.select("text")
//                 .style("fill-opacity", 1);

//             // Transition exiting nodes to the parent's new position.
//             let nodeExit = node.exit().transition()
//                 .duration(duration)
//                 .attr("transform", (d) => { return "translate(" + source.x + "," + source.y + ")"; })
//                 .remove();
//             nodeExit.select("circle")
//                 .attr("r", 1e-6);
//             nodeExit.select("text")
//                 .style("fill-opacity", 1e-6);
//             // Update the links…
//             // Declare the links…
//             let link = svg.selectAll("path.link")
//                 .data(links, (d) => { return d.target.id; });
//             // Enter the links.
//             link.enter().insert("path", "g")
//                 .attr("class", "link")
//                 .attr("marker-end", (d) => {
//                     // console.log(d);
//                     if (d.source.children.length == 1) {
//                         return "url(#marker_straight)";
//                     }

//                     if (d.source.children[0] == d.target) {
//                         return "url(#marker_left)";
//                     }

//                     return "url(#marker_right)";

//                 })
//                 .attr("d", (d) => {

//                     if (source.x0 && source.y0) {

//                         let o = { x: source.x0, y: source.y0 };
//                         return diagonal({ source: o, target: o });
//                     }
//                     let o = { x: source.x, y: source.y };
//                     return diagonal({ source: o, target: o });
//                 })
//             // Transition links to their new position.
//             link.transition()
//                 .duration(duration)
//                 .attr("d", diagonal);
//             // Transition exiting nodes to the parent's new position.
//             link.exit().transition()
//                 .duration(duration)
//                 .attr("d", (d) => {
//                     let o = { x: source.x, y: source.y };
//                     return diagonal({ source: o, target: o });
//                 })
//                 .remove();

//             // Stash the old positions for transition.
//             nodes.forEach((d) => {
//                 d.x0 = d.x;
//                 d.y0 = d.y;
//             });
//         }

//         function showDomains(minionID) {

//             let checked = (document.getElementById('check').checked);
//             panel.setHeaderTitle("Status at Node: " + minionID);

//             if (checked) {

//                 let setAttributeList = ["Cardinality", "Excluded", "Included"];

//                 function unselectAll() {
//                     let selected = treeView.treeview('getSelected');
//                     selected.forEach(element => {
//                         treeView.treeview('unselectNode', [element, { silent: true }]);
//                     });
//                 }

//                 function selectVariable(diff, m) {

//                     if (setAttributeList.includes(m.text)) {
//                         treeView.treeview('selectNode', [m.nodeId, { silent: true }]);
//                     }

//                     if (m.text in normalDomainMap[minionID]) {

//                         let variable = normalDomainMap[minionID][m.text];

//                         if (!("table" in variable)) {
//                             treeView.treeview('selectNode', [m.nodeId, { silent: true }]);
//                         }
//                     }

//                     if (!(typeof diff === 'string')) {

//                         for (const key in diff) {
//                             if (key != "_t") {
//                                 selectVariable(diff[key], m[key]);
//                             }
//                         }
//                     }
//                 }

//                 function selectChanged() {

//                     unselectAll();

//                     if (minionID != 1) {

//                         let delta = jsondiffpatch.diff(treeviewDomainMap[minionID - 1], treeviewDomainMap[minionID]);

//                         if (delta) {
//                             selectVariable(delta, allTreeViewNodes);
//                         }
//                     }
//                 }

//                 let expandedNodes;
//                 let treeView = $('#pane');


//                 if (!init) {
//                     expandedNodes = treeView.treeview('getExpanded');
//                 }

//                 $("#pane").empty();

//                 $('#pane').treeview({
//                     expandIcon: "glyphicon glyphicon-triangle-right",
//                     collapseIcon: "glyphicon glyphicon-triangle-bottom",
//                     color: "white",
//                     backColor: colours.bgColour,
//                     onhoverColor: "purple",
//                     borderColor: colours.bgColour,
//                     showBorder: false,
//                     showTags: true,
//                     highlightSelected: true,
//                     multiSelect: true,
//                     selectedColor: "black",
//                     selectedBackColor: "coral",
//                     data: treeviewDomainMap[minionID],
//                     onNodeSelected: function (event, node) {
//                         treeView.treeview('unselectNode', [node.nodeId, { silent: true }]);
//                         selectChanged();
//                         // console.log(node);
//                     },
//                     // onSearchComplete: (event, node) => {
//                     //     // console.log(node);
//                     // }
//                 });

//                 if (init) {
//                     treeView.treeview('expandAll', { levels: 10000000001, silent: true });
//                     expandedNodes = treeView.treeview('getExpanded');
//                     allTreeViewNodes = expandedNodes;
//                     init = false;
//                 }

//                 expandedNodes.forEach(element => {
//                     $('#pane').treeview('expandNode', [element.nodeId, { levels: 1, silent: true }]);
//                 })

//                 selectChanged();
//             }
//             else {
//                 $("#pane").empty();
//                 tabulate(simpleDomainMap[minionID], ['name', 'range']);

//                 if (minionID != 1) {
//                     let delta = jsondiffpatch.diff(simpleDomainMap[minionID - 1], simpleDomainMap[minionID]);
//                     for (const key in delta) {
//                         if (key != "_t") {
//                             let changedVar = simpleDomainMap[minionID][Number(key)];
//                             $("#row" + key).toggleClass("changed");
//                         }
//                     }
//                 }

//             }
//         }

//         function selectNode(minionID) {
//             if (selectedCircle) {
//                 selectedCircle.classed("selected", false);
//             }
//             selectedCircle = d3.select("#node" + minionID).select("circle");
//             selectedCircle.classed("selected", true);
//             selectedNode = Number(minionID);
//             showDomains(selectedNode);
//         }

//         // Toggle children on click.
//         function nodeToggle(d) {
//             if (d.children) {
//                 d._children = d.children;
//                 d.children = null;
//             } else {
//                 // console.log("Children expanded!!!");
//                 d.children = d._children;
//                 d._children = null;
//             }
//             update(d);
//         }

//         // Control Controllers ------------------------

//         function walkTree(d, collapse) {

//             if (collapse) {
//                 if (d.children) {
//                     d._children = d.children;
//                     d.children = null;
//                 }

//                 if (d._children) {
//                     d._children.forEach(child => {
//                         walkTree(child, collapse);
//                     });
//                 }
//             }
//             else {
//                 if (d._children) {
//                     d.children = d._children;
//                     d._children = null;
//                 }

//                 if (d.children) {
//                     d.children.forEach(child => {
//                         walkTree(child, collapse);
//                     });
//                 }
//             }
//         }

//         function collapser() {
//             walkTree(root, true);
//             update(root);
//             focusNode(root);
//             selectNode(root.minionID);
//         }

//         function expander() {
//             walkTree(root, false);
//             update(root);
//         }

//         update(root);
//         selectNode(root.minionID);
//         focusNode(globals.id2Node[root.minionID]);
//     });
// }())

// function tabulate(data, columns) {
//     var table = d3.select('#pane').append('table')
//     var thead = table.append('thead')
//     var tbody = table.append('tbody');

//     // append the header row
//     thead.append('tr')
//         .selectAll('th')
//         .data(columns).enter()
//         .append('th')
//         .text(function (column) { return column; });

//     // create a row for each object in the data
//     var rows = tbody.selectAll('tr')
//         .data(data)
//         .enter()
//         .append('tr')
//         .attr("id", (d, i) => { return "row" + i })

//     // create a cell in each row for each column
//     var cells = rows.selectAll('td')
//         .data((row) => {
//             return columns.map((column) => {
//                 return { column: column, value: row[column] };
//             });
//         })
//         .enter()
//         .append('td')
//         .text((d) => { return d.value; });

//     return table;
// }