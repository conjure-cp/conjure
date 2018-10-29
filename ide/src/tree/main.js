console.log("Start!!!!");

let bgColour = getComputedStyle(document.body).getPropertyValue('--background-color');

let panel = jsPanel.create({
    theme: bgColour + ' filled',
    headerTitle: 'my panel #1',
    position: 'right-top 0 58',
    contentSize: {
        // width: 450,
        width: () => {
            return $(document).width() / 3;
        },
        height: () => {
            return $(document).height() * 0.9;
        }
    },
    content: '<div id="pane"> </div>'
});

(function () {

    const vscode = acquireVsCodeApi();

    vscode.postMessage({
        command: 'ready',
    })

    window.addEventListener('message', event => {

        const root = event.data.tree; // The JSON data our extension sent
        const domainMap = event.data.map; // The JSON data our extension sent
        let nodeMap = {};
        let selectedNode;
        let selectedCircle;
        let init = true;

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
                selectedNode = (selectedNode - 1) % (Object.keys(domainMap).length + 1);
                if (selectedNode == 0) {
                    selectedNode = Object.keys(domainMap).length;
                }
                selectNode(selectedNode);
                focusNode(nodeMap[selectedNode]);
            });

        d3.select("#controls")
            .append("input")
            .attr("type", "button")
            .attr("value", "Next")
            .on("click", () => {
                selectedNode = (selectedNode + 1) % (Object.keys(domainMap).length + 1);
                if (selectedNode == 0) {
                    selectedNode++;
                }
                selectNode(selectedNode);
                focusNode(nodeMap[selectedNode]);
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

        function zoomed() {
            svg.attr("transform", "translate(" + d3.event.translate + ")" + " scale(" + d3.event.scale + ")")
            // zoom.translate(d3.event.translate);

        }

        function focusNode(node) {
            scale = zoom.scale();
            let x = -node.x * scale;
            let y = -node.y * scale;


            x += width/3;
            y += height/2;

            // let x = viewerWidth / 3 - node.x;
            // let y = (200 * scale) - node.y;

            


            // x = 400;
            // y = 400;


            console.log("scale: ", scale);
            console.log("moving x: ", x);
            console.log("moving y: ", y);

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
                    return "translate(" + source.x0 + "," + source.y0 + ")";
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
            //.attr("r", 10)
            //.style("fill", "#fff");
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
                .attr("d", (d) => {
                    let o = { x: source.x0, y: source.y0 };
                    return diagonal({ source: o, target: o });
                });
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

            panel.setHeaderTitle("Status at Node: " + minionID);
            let treeView = $('#pane');
            let expandedNodes = treeView.treeview('getExpanded');

            $("#pane").empty();

            $('#pane').treeview({
                expandIcon: "glyphicon glyphicon-triangle-right",
                collapseIcon: "glyphicon glyphicon-triangle-bottom",
                color: "white",
                backColor: bgColour,
                onhoverColor: "purple",
                borderColor: bgColour,
                showBorder: false,
                showTags: true,
                highlightSelected: false,
                selectedColor: "blue",
                selectedBackColor: bgColour,
                data: domainMap[minionID]
            });

            if (init) {
                $('#pane').treeview('expandAll', { levels: 10000000001, silent: true });
                let treeView = $('#pane');
                expandedNodes = treeView.treeview('getExpanded');
                init = false;
            }

            expandedNodes.forEach(element => {
                $('#pane').treeview('expandNode', [element.nodeId, { levels: 1, silent: true }]);
            })

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
        selectNode(root.minionID);
        focusNode(root);
    });
}())