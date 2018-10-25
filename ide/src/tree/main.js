console.log("Start!!!!");

let bgColour = getComputedStyle(document.body).getPropertyValue('--background-color');

let panel = jsPanel.create({
    theme: bgColour + ' filled',
    headerTitle: 'my panel #1',
    position: 'bottom-right 0 58',
    contentSize: {
        // width: 450,
        width: () => {
            return $(document).width() / 3;
        },
        height: () => {
            return $(document).height();
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

        const root = event.data; // The JSON data our extension sent

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
                focusNode(root);
            });

        d3.select("#controls")
            .append("input")
            .attr("type", "button")
            .attr("value", "Play")
            .on("click", () => {
                duration = 0;
                function p(node) {
                    // console.log(node);
                    d3.select(".node circle")
                        .transition()
                        // .duration(1000)
                        .style("fill", "red");
                    nodeclick(node);
                    return;

                    // console.log(d3.select(node));
                    // d3.select(node).property("delay","500");
                    // console.log(d3.select(node));
                    // nodeclick(node);
                    if (node.children) {
                        node.children.forEach(child => {
                            p(child);
                        });
                    }
                }
                collapser();
                p(root);
            });

        // set the dimensions and margins of the diagram
        let viewerWidth = $(document).width();
        let viewerHeight = $(document).height();
        let margin = { top: 40, right: 30, bottom: 50, left: 30 },
            width = viewerWidth - margin.left - margin.right,
            height = viewerHeight - margin.top - margin.bottom;

        let i = 0, duration = 750;
        let tree = d3.layout.tree()
            .size([height, width]);
        let diagonal = d3.svg.diagonal()
            .projection((d) => { return [d.x, d.y]; });


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

        }

        function focusNode(node) {
            scale = zoom.scale();

            let x = viewerWidth / 3 - node.x;
            let y = 200 - node.y;

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
                .attr("transform", (d) => {
                    return "translate(" + source.x0 + "," + source.y0 + ")";
                })
                .on("click", nodeclick)
                .on("mouseover", showDomains);

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

        let remember = [];

        function showDomains(d) {

            panel.setHeaderTitle("State at Node: " + d.minionID);
            $("#pane").empty();
            // tabulate(d.Domains, ['name', 'range']);

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
                data: d.Domains,
                onNodeExpanded: (event, data) => {
                    console.log(data);
                    if (!remember.includes(data.nodeId)) {
                        remember.push(data.nodeId);
                    }
                },

                onNodeCollapsed: (event, data) => {
                    remember.splice(remember.indexOf(data.nodeId), 1)
                }
            });

            $('#pane').treeview('collapseAll', { silent: true });

            console.log(remember);

            remember.forEach(element => {
                console.log(element);
                $('#pane').treeview('expandNode', [element, { levels: 1, silent: true }]);

            })

        }

        // Toggle children on click.
        function nodeclick(d) {
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
            // focusNode(root);
        }

        function expander() {
            walkTree(root, false);
            update(root);
            // focusNode(root);
        }

        update(root);
        focusNode(root);

    });
}())