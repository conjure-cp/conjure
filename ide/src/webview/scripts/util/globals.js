exports.vscode = acquireVsCodeApi();
exports.totalLoaded = 0
exports.selectedId = 1;
exports.currentDomainId = 0;
exports.id2Node = {};
exports.id2Parent = {};
exports.id2ChildIds = {};
exports.correctPath = [];
exports.viewerWidth = $(document).width();
exports.viewerHeight = $(document).height();
exports.margin = { top: 40, right: 30, bottom: 50, left: 30 };
exports.width = exports.viewerWidth - exports.margin.left - exports.margin.right;
exports.height = exports.viewerHeight - exports.margin.top - exports.margin.bottom;

exports.pathList = []
exports.simpleDomainsAtRoot;
exports.init = true;
exports.pretty = true;
exports.frozen = false;
exports.i = 0;
exports.duration = 750;
exports.tree = d3.layout.tree()
    .size([exports.height, exports.width])
// .nodeSize([300, 100]);

exports.waiting = false;

let z = -1;
let columns = ["name", "rng"]

exports.setup = (zoom) => {
    z = zoom;
}

exports.diagonal = d3.svg.diagonal()
    .projection((d) => {
        return [d.x, d.y];

    });


exports.expandNode = (nodeId) => {

    function recurse(node) {

        for (i in node._children) {
            recurse(node._children[i])
        }

        showChildren(node.id);
    }

    let node = exports.id2Node[nodeId];
    recurse(node);
}


exports.collapseNode = (nodeId) => {


    function recurse(node) {

        for (i in node.children) {
            recurse(node.children[i])
        }

        hideChildren(node.id);
    }

    let node = exports.id2Node[nodeId];
    recurse(node);

}

function showChildren(nodeId) {


    if (exports.id2Node[nodeId]) {
        if (exports.id2Node[nodeId]._children) {
            exports.id2Node[nodeId].children = exports.id2Node[nodeId]._children;
            exports.id2Node[nodeId]._children = null;
        }
    }
}

function hideChildren(nodeId) {
    // console.log(nodeId);
    // console.log(exports.id2Node);
    // console.log(exports.id2Node[nodeId]);
    if (exports.id2Node[nodeId]) {
        if (exports.id2Node[nodeId].children) {
            exports.id2Node[nodeId]._children = exports.id2Node[nodeId].children;
            exports.id2Node[nodeId].children = null;
        }
    }
}


exports.toggleNode = (nodeId) => {

    if (exports.id2Node[nodeId]._children) {
        showChildren(nodeId);
    }
    else if (exports.id2Node[nodeId].children) {
        hideChildren(nodeId);
    }
}

exports.focusNode = (node) => {
    // scale = 7;''
    // console.log(node);
    let scale = z.scale();
    let x = -node.x * scale;
    let y = -node.y * scale;

    x += exports.width / 3;
    y += exports.height / 2;

    d3.select('g').transition()
        .duration(exports.duration)
        .attr("transform", "translate(" + x + "," + y + ")scale(" + scale + ")");
    z.translate([x, y]);
}


exports.collapseFailed = () => {
    exports.correctPath.forEach(nodeId => {
        if (exports.id2ChildIds[nodeId]) {
            exports.id2ChildIds[nodeId].forEach(childId => {
                if (!exports.correctPath.includes(childId)) {
                    exports.collapseNode(childId);
                }
            })
        }

    });

    if (!exports.correctPath.includes(exports.selectedId)) {

        for (var i = 0; i < exports.correctPath.length; i++) {

            let nodeId = exports.correctPath[i];

            if (nodeId > exports.selectedId) {

                exports.selectedId = nodeId;

                break;
            }
        };
    }
}

exports.rightNode = () => {
    if (exports.id2Node[exports.selectedId].children) {
        let childCount = exports.id2Node[exports.selectedId].children.length;
        if (childCount > 1) {
            exports.selectedId = exports.id2Node[exports.selectedId].children[childCount - 1].id;
        }
        exports.selectNode(exports.selectedId);
    }
}

exports.nextNode = () => {

    // console.log(exports.waiting);

    let stepSize = Number($("#stepSize").val());

    // if (stepSize > exports.correctPath[exports.correctPath.length-1]) {
    //     stepSize = exports.correctPath[exports.correctPath.length-1] - exports.selectedId;
    // }

    // console.log(exports.selectedId + stepSize);
    // console.log(exports.id2Node);
    // console.log(exports.id2Node[exports.selectedId + stepSize]);


    if (exports.id2Node[exports.selectedId]._children) {
        // console.log("NOW")
        exports.toggleNode(exports.selectedId);
        return
    }

    if (!exports.id2Node[exports.selectedId + stepSize]) {
        exports.loadNNodes();
        // console.log("NEXT")
    }
    else {
        // exports.selectedId = exports.currentId;
        console.log("here!!!")
        exports.selectedId += stepSize;
        exports.selectNode(exports.selectedId);
    }
}


//TODO check that the previous node is not withn a collapsed node
exports.previousNode = () => {
    if (exports.selectedId > 1) {
        exports.selectedId--;
        exports.selectNode(exports.selectedId);
    }
}

exports.upNode = () => {
    if (exports.selectedId > 1) {
        exports.selectedId = exports.id2Parent[exports.selectedId].id;
    }
    exports.selectNode(exports.selectedId);
}

exports.loadNNodes = () => {

    if (!exports.waiting) {

        // console.log("requesting more");

        exports.vscode.postMessage({
            command: 'loadNodes',
            amount: Number($("#stepSize").val()),
            start: exports.selectedId
            // start: exports.currentId
        });

        // console.log("SET WAIT TRUE NODES");
        exports.waiting = true;
    }
    else {
        // console.log("waiting");

    }
}

exports.selectNode = (nodeId) => {
    exports.selectedId = nodeId;

    let allCircles = ".node circle"
    d3.selectAll(allCircles).classed("selected", false);
    let s = "#node" + nodeId + " circle";
    d3.select(s).classed("selected", true);

    // console.log(nodeId)
    // console.log(exports.id2Node[nodeId])

    exports.focusNode(exports.id2Node[nodeId]);


    exports.currentDomainId = 0;

    // console.log("Calling load domains");
    // if (!exports.pretty) {
    //     $("#pane").empty();
    //     exports.tabulate()
    // }

    if (!exports.frozen) {
        exports.loadDomains();
    }

}

exports.loadDomains = () => {

    // console.log(exports.waiting);

    if (!exports.waiting) {

        if (exports.pretty) {
            exports.sendPrettyRequest()
        }
        else {
            exports.sendSimpleRequest()
        }

        exports.waiting = true;
    }
    // console.log("SET WAIT TRUE DOMAINS, pretty: " + exports.pretty );
}
exports.sendSimpleRequest = () => {
        exports.vscode.postMessage({
            command: "simpleDomains",
            amount: Number($("#domCount").val()),
            start: exports.currentDomainId,
            nodeId: exports.selectedId,
        });
}

exports.sendPrettyRequest = () => {
        exports.vscode.postMessage({
            command: "prettyDomains",
            nodeId: exports.selectedId,
            paths: exports.pathList.join(":")
        });
}



exports.tabulate = () => {
    var table = d3.select('#pane').append('table')
    var thead = table.append('thead')

    // append the header row
    thead.append('tr')
        .selectAll('th')
        .data(columns).enter()
        .append('th')
        .text(function (column) { return column; });
}

exports.appendRows = (data) => {
    var table = d3.select('#pane').append('table');
    var tbody = table.append('tbody');

    var rows = tbody.selectAll('tr')
        .data(data)
        .enter()
        .append('tr')
        .attr("id", (d, i) => {
            // console.log(d);
            // return "row" + (i + exports.currentDomainId - Number($("#domCount").val())) 
            return d.name;
        })

    // create a cell in each row for each column
    var cells = rows.selectAll('td')
        .data((row) => {
            return columns.map((column) => {
                return { column: column, value: row[column] };
            });
        })
        .enter()
        .append('td')
        .text((d) => { return d.value; });
}

exports.addNode = (nodeId, parentId, label) => {

    exports.totalLoaded++;
    let newNode = { id: nodeId, name: label };
    // console.log(exports.currentId);
    // console.log(parentId);
    // console.log(exports.id2Node);

    if (parentId === -1) {
        exports.id2Node[nodeId] = newNode;
        return;
    }

    if (!exports.id2Node[parentId].children) {
        exports.id2Node[parentId].children = [];
    }

    exports.id2Node[parentId].children.push(newNode);
    exports.id2Node[nodeId] = newNode;
    exports.id2Parent[nodeId] = exports.id2Node[parentId];
}

exports.getChildren = (parentId) => {
    exports.vscode.postMessage({
        command: 'children',
        parentId: parentId,
    });
}

exports.vscode.postMessage({
    command: 'init',
});


exports.vscode.postMessage({
    command: 'correctPath',
});

exports.vscode.postMessage({
    command: 'longestBranchingVariable',
});

exports.vscode.postMessage({
    command: 'loadCore',
});

exports.vscode.postMessage({
    command: 'simpleDomains',
    amount: Number($("#domCount").val()),
    start: 0,
    nodeId: 1,
});
