let panel = require('./util/panel.js');
let colours = require('./util/colours.js');

(function () {

    const vscode = acquireVsCodeApi();

    vscode.postMessage({
        command: 'ready',
    })

    window.addEventListener('message', event => {

        const root = event.data.tree; // The JSON data our extension sent
        const treeviewDomainMap = event.data.treeviewDomainMap; // The JSON data our extension sent
        const normalDomainMap = event.data.normalDomainMap; // The JSON data our extension sent

        let parentMap = {};
        let nodeMap = {};
        let selectedNode;
        let selectedCircle;
        let init = true;
        let allTreeViewNodes;

        Mousetrap.bind('n', next, 'keydown')
        Mousetrap.bind('p', previous, 'keydown')
        Mousetrap.bind('r', goRight, 'keydown')
        Mousetrap.bind('l', goLeft, 'keydown')
        Mousetrap.bind('u', goUp, 'keydown')

        function goUp() {

            let nextNode;

            if (selectedNode in parentMap) {
                nextNode = parentMap[selectedNode];
            }
            else {
                for (const key in nodeMap) {
                    if (nodeMap[key].children) {
                        if (nodeMap[key].children.includes(nodeMap[selectedNode])) {
                            nextNode = nodeMap[key];
                            break;
                        }
                    }
                }
            }

            parentMap[selectedNode] = nextNode;
            selectedNode = nextNode.minionID;

            selectNode(selectedNode);
            focusNode(nodeMap[selectedNode]);
        }

        function goLeft() {
            if (nodeMap[selectedNode].children.length > 0) {
                selectedNode = nodeMap[selectedNode].children[0].minionID;
                selectNode(selectedNode);
                focusNode(nodeMap[selectedNode]);
            }
        }

        function goRight() {
            if (nodeMap[selectedNode].children.length == 2) {
                selectedNode = nodeMap[selectedNode].children[1].minionID;
                selectNode(selectedNode);
                focusNode(nodeMap[selectedNode]);
            }
        }

        function next() {
            selectedNode = (selectedNode + 1) % (Object.keys(treeviewDomainMap).length + 1);
            if (selectedNode == 0) {
                selectedNode++;
            }
            selectNode(selectedNode);
            focusNode(nodeMap[selectedNode]);
        }

        function previous() {
            selectedNode = (selectedNode - 1) % (Object.keys(treeviewDomainMap).length + 1);
            if (selectedNode == 0) {
                selectedNode = Object.keys(treeviewDomainMap).length;
            }
            selectNode(selectedNode);
            focusNode(nodeMap[selectedNode]);
        }

        // Add controls
        d3.select("#controls")
            .append("input")
            .attr("type", "button")
            .attr("value", "Collapse All")
            .on("click", collapser);

        d3.select("#controls")
            .append("input")
            .attr("type", "button")
            .attr("value", "Expand All")
            .on("click", expander);

        d3.select("#controls")
            .append("input")
            .attr("type", "button")
            .attr("value", "Find Root")
            .on("click", () => {
                selectNode(root.minionID);
                focusNode(nodeMap[root.minionID]);
            });

        d3.select("#controls")
            .append("input")
            .attr("type", "button")
            .attr("value", "Previous")
            .on("click", () => {
                previous();
            });

        d3.select("#controls")
            .append("input")
            .attr("type", "button")
            .attr("value", "Next")
            .on("click", () => {
                next();
            });

        d3.select("#controls")
            .append("input")
            .attr("type", "button")
            .attr("value", "Toggle")
            .on("click", () => {
                nodeToggle(nodeMap[selectedNode]);
            });


        // set the dimensions and margins of the diagram
        let viewerWidth = $(document).width();
        let viewerHeight = $(document).height();
        let margin = { top: 40, right: 30, bottom: 50, left: 30 },
            width = viewerWidth - margin.left - margin.right,
            height = viewerHeight - margin.top - margin.bottom;

        let i = 0, duration = 750;
        let tree = d3.layout.tree().size([height, width]);

        let diagonal = d3.svg.diagonal()
            .projection((d) => {
                return [d.x, d.y];

            });

        let zoom = d3.behavior.zoom()
            .on("zoom", zoomed);

        let svg = d3.select("#tree")
            .append("svg")
            .call(zoom)
            .attr("width", viewerWidth)
            .attr("height", viewerHeight)
            .append("g")


        var data = [
            { name: 'left', orient: "134", refX: 20, refY: -4, colour: "red" },
            { name: 'right', orient: "40", refX: 20, refY: 5.5, colour: "orange" },
            { name: 'straight', orient: "auto", refX: 20, refY: 0, colour: "yellow" }
        ]


        svg.append("svg:defs").selectAll("marker")
            .data(data)      // Different link/path types can be defined here
            .enter()
            .append("svg:marker")    // This section adds in the arrows
            .attr('id', function (d) { return 'marker_' + d.name })
            .attr("viewBox", "0 -5 10 10")
            .attr("orient", (d) => { return d.orient })
            .attr("refX", (d) => { return d.refX })
            .attr("refY", (d) => { return d.refY })
            .attr("markerWidth", 10)
            .attr("markerHeight", 6)
            .append("svg:path")
            .attr("d", "M0, -5L10, 0L0, 5")
            .style("fill", (d) => { return "#0dbc79" });

        function zoomed() {
            svg.attr("transform", "translate(" + d3.event.translate + ")" + " scale(" + d3.event.scale + ")")
            // zoom.translate(d3.event.translate);

        }

        function focusNode(node) {
            // scale = 7;
            scale = zoom.scale();
            let x = -node.x * scale;
            let y = -node.y * scale;

            x += width / 3;
            y += height / 2;

            d3.select('g').transition()
                .duration(duration)
                .attr("transform", "translate(" + x + "," + y + ")scale(" + scale + ")");
            zoom.translate([x, y]);
        }


        function update(source) {
            // Compute the new tree layout.
            let nodes = tree.nodes(root).reverse(),
                links = tree.links(nodes);

            // Normalize for fixed-depth.
            nodes.forEach((d) => { d.y = d.depth * 100; });
            // Declare the nodes…
            let node = svg.selectAll("g.node")
                .data(nodes, (d) => { return d.id || (d.id = ++i); });
            // Enter the nodes.
            let nodeEnter = node.enter().append("g")
                .attr("class", "node")
                .attr("id", (d) => {
                    return "node" + d.minionID;
                })
                .attr("transform", (d) => {
                    nodeMap[Number(d.minionID)] = d;
                    if (source.x0 && source.y0) {
                        return "translate(" + source.x0 + "," + source.y0 + ")";
                    }
                })
                .on("click", (d) => {
                    selectNode(d.minionID);
                });
            // .on("mouseover", showDomains);

            nodeEnter.append("circle")
                .transition()
                .attr("r", 10)
                .attr("stroke", (d) => { return d.children || d._children ? "steelblue" : "#00c13f"; })
                .style("fill", (d) => { return d.children || d._children ? "#6ac4a1" : "#fff"; });

            nodeEnter.append("text")
                .attr("y", (d) => {
                    if (d.children || d._children) {
                        return 42;
                    }
                    return 20;
                })
                .attr("dy", ".35em")
                .attr("text-anchor", "middle")
                .text((d) => { return d.name; })
                .style("fill-opacity", 1e-6);
            // Transition nodes to their new position.
            //horizontal tree
            let nodeUpdate = node.transition()
                .duration(duration)
                .attr("transform", (d) => { return "translate(" + d.x + "," + d.y + ")"; });
            nodeUpdate.select("circle")
                .attr("r", 10)
                .style("fill", (d) => { return d._children ? "#6ac4a1" : "#fff"; });
            nodeUpdate.select("text")
                .style("fill-opacity", 1);


            // Transition exiting nodes to the parent's new position.
            let nodeExit = node.exit().transition()
                .duration(duration)
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
                .attr("marker-end", (d) => {
                    // console.log(d);
                    if (d.source.children.length == 1) {
                        return "url(#marker_straight)";
                    }

                    if (d.source.children[0] == d.target) {
                        return "url(#marker_left)";
                    }

                    return "url(#marker_right)";

                })
                .attr("d", (d) => {

                    if (source.x0 && source.y0) {

                        let o = { x: source.x0, y: source.y0 };
                        return diagonal({ source: o, target: o });
                    }
                })
            // Transition links to their new position.
            link.transition()
                .duration(duration)
                .attr("d", diagonal);
            // Transition exiting nodes to the parent's new position.
            link.exit().transition()
                .duration(duration)
                .attr("d", (d) => {
                    let o = { x: source.x, y: source.y };
                    return diagonal({ source: o, target: o });
                })
                .remove();

            // Stash the old positions for transition.
            nodes.forEach((d) => {
                d.x0 = d.x;
                d.y0 = d.y;
            });
        }

        function showDomains(minionID) {

            let setAttributeList = ["Cardinality", "Excluded", "Included"];

            function unselectAll() {
                let selected = treeView.treeview('getSelected');
                selected.forEach(element => {
                    treeView.treeview('unselectNode', [element, { silent: true }]);
                });
            }

            function selectVariable(diff, m) {

                if (setAttributeList.includes(m.text)) {
                    treeView.treeview('selectNode', [m.nodeId, { silent: true }]);
                }

                if (m.text in normalDomainMap[minionID]) {

                    let variable = normalDomainMap[minionID][m.text];

                    if (!("table" in variable)) {
                        treeView.treeview('selectNode', [m.nodeId, { silent: true }]);
                    }
                }

                if (!(typeof diff === 'string')) {

                    for (const key in diff) {
                        if (key != "_t") {
                            selectVariable(diff[key], m[key]);
                        }
                    }
                }
            }

            function selectChanged() {

                unselectAll();

                if (minionID != 1) {

                    let delta = jsondiffpatch.diff(treeviewDomainMap[minionID - 1], treeviewDomainMap[minionID]);

                    if (delta) {
                        selectVariable(delta, allTreeViewNodes);
                    }
                }
            }

            let expandedNodes;
            let treeView = $('#pane');

            panel.setHeaderTitle("Status at Node: " + minionID);

            if (!init) {
                expandedNodes = treeView.treeview('getExpanded');
            }

            $("#pane").empty();

            $('#pane').treeview({
                expandIcon: "glyphicon glyphicon-triangle-right",
                collapseIcon: "glyphicon glyphicon-triangle-bottom",
                color: "white",
                backColor: colours.bgColour,
                onhoverColor: "purple",
                borderColor: colours.bgColour,
                showBorder: false,
                showTags: true,
                highlightSelected: true,
                multiSelect: true,
                selectedColor: "black",
                selectedBackColor: "coral",
                data: treeviewDomainMap[minionID],
                onNodeSelected: function (event, node) {
                    treeView.treeview('unselectNode', [node.nodeId, { silent: true }]);
                    selectChanged();
                    // console.log(node);
                },
                // onSearchComplete: (event, node) => {
                //     // console.log(node);
                // }
            });

            if (init) {
                treeView.treeview('expandAll', { levels: 10000000001, silent: true });
                expandedNodes = treeView.treeview('getExpanded');
                allTreeViewNodes = expandedNodes;
                init = false;
            }

            expandedNodes.forEach(element => {
                $('#pane').treeview('expandNode', [element.nodeId, { levels: 1, silent: true }]);
            })

            selectChanged();
        }

        function selectNode(minionID) {
            if (selectedCircle) {
                selectedCircle.classed("selected", false);
            }
            selectedCircle = d3.select("#node" + minionID).select("circle");
            selectedCircle.classed("selected", true);
            selectedNode = Number(minionID);
            showDomains(selectedNode);
        }

        // Toggle children on click.
        function nodeToggle(d) {
            if (d.children) {
                d._children = d.children;
                d.children = null;
            } else {
                // console.log("Children expanded!!!");
                d.children = d._children;
                d._children = null;
            }
            update(d);
        }

        // Control Controllers ------------------------

        function walkTree(d, collapse) {

            if (collapse) {
                if (d.children) {
                    d._children = d.children;
                    d.children = null;
                }

                if (d._children) {
                    d._children.forEach(child => {
                        walkTree(child, collapse);
                    });
                }
            }
            else {
                if (d._children) {
                    d.children = d._children;
                    d._children = null;
                }

                if (d.children) {
                    d.children.forEach(child => {
                        walkTree(child, collapse);
                    });
                }
            }
        }

        function collapser() {
            walkTree(root, true);
            update(root);
            focusNode(root);
        }

        function expander() {
            walkTree(root, false);
            update(root);
            focusNode(root);
        }

        update(root);
        // selectNode(root.minionID);
        selectNode(1);
        focusNode(nodeMap[1]);
    });
}())