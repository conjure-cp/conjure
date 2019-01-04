exports.vscode = acquireVsCodeApi();
exports.currentId = 0;
exports.selectedId = 1;
exports.currentDomainId = 0;
exports.id2Node = {};
exports.id2Parent = {};
exports.id2ChildIds = {};
exports.viewerWidth = $(document).width();
exports.viewerHeight = $(document).height();
exports.margin = { top: 40, right: 30, bottom: 50, left: 30 };
exports.width = exports.viewerWidth - exports.margin.left - exports.margin.right;
exports.height = exports.viewerHeight - exports.margin.top - exports.margin.bottom;

exports.init = true;
exports.pretty = true;
exports.i = 0;
exports.duration = 750;
exports.tree = d3.layout.tree().size([exports.height, exports.width]);

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

    showChildren(nodeId);
    let kidsToExpand = exports.id2ChildIds[nodeId];

    while (kidsToExpand && kidsToExpand.length > 0) {

        let nextKids = [];

        kidsToExpand.forEach((id) => {
            nextKids = exports.id2ChildIds[id];
            showChildren(id);
        });

        kidsToExpand = nextKids;
    }

}


exports.collapseNode = (nodeId) => {

    hideChildren(nodeId);
    let kidsToCollapse = exports.id2ChildIds[nodeId];

    while (kidsToCollapse && kidsToCollapse.length > 0) {

        let nextKids = [];

        kidsToCollapse.forEach((id) => {
            nextKids = exports.id2ChildIds[id];
            hideChildren(id);
        });

        kidsToCollapse = nextKids;
    }
}

function showChildren(nodeId) {


    if (exports.id2Node[nodeId]) {
        if (exports.id2Node[nodeId]._children) {
            exports.id2Node[nodeId].children = exports.id2Node[nodeId]._children;
            exports.id2Node[nodeId]._children = null;
            console.log("collapsed " + nodeId);
        }
    }
}

function hideChildren(nodeId) {
    console.log(nodeId);
    console.log(exports.id2Node);
    console.log(exports.id2Node[nodeId]);
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
    let stepSize = Number($("#stepSize").val());

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
    }
    else {
        // exports.selectedId = exports.currentId;
        exports.selectedId += stepSize;
        exports.selectNode(exports.selectedId);
    }
}

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

        exports.vscode.postMessage({
            command: 'loadNodes',
            amount: Number($("#stepSize").val()),
            start: exports.currentId
        });

        exports.waiting = true;
    }
}

exports.selectNode = (nodeId) => {
    exports.selectedId = nodeId;
    let allCircles = ".node circle"
    d3.selectAll(allCircles).classed("selected", false);
    let s = "#node" + nodeId + " circle";
    d3.select(s).classed("selected", true);
    exports.focusNode(exports.id2Node[nodeId]);


    exports.currentDomainId = 0;
    exports.loadDomains(nodeId);

    if (!exports.pretty){
        $("#pane").empty();
        exports.tabulate()
    }
}

exports.loadDomains = (nodeId) => {
    if (!exports.waiting) {

        let commandString;

        if (exports.pretty) {
            commandString = "prettyDomains"
        }
        else {
            commandString = "simpleDomains"
        }

        exports.vscode.postMessage({
            command: commandString,
            amount: Number($("#domCount").val()),
            start: exports.currentDomainId,
            nodeId: nodeId,
        });
    }
    exports.waiting = true;
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
        .attr("id", (d, i) => { return "row" +  (i + exports.currentDomainId - Number($("#domCount").val())) })

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

exports.addNode = (parentId) => {

    exports.currentId++;
    let newNode = { id: exports.currentId };
    // console.log(exports.currentId);
    // console.log(parentId);
    // console.log(exports.id2Node);

    if (parentId === -1) {
        exports.id2Node[exports.currentId] = newNode;
        return;
    }

    if (!exports.id2Node[parentId].children) {
        exports.id2Node[parentId].children = [];
    }

    exports.id2Node[parentId].children.push(newNode);
    exports.id2Node[exports.currentId] = newNode;
    exports.id2Parent[exports.currentId] = exports.id2Node[parentId];
}

exports.getChildren = (parentId) => {
    exports.vscode.postMessage({
        command: 'children',
        parentId: parentId,
    });
}

exports.setHasOthers = (nodeId) => {
    let s = "#node" + nodeId + " circle";
    d3.select(s).classed("hasOthers", true);
    // console.log("SEtting has others for " + nodeId)
}

exports.unsetHasOthers = (nodeId) => {
    let s = "#node" + nodeId + " circle";
    d3.select(s).classed("hasOthers", false);
}

exports.vscode.postMessage({
    command: 'init',
});

exports.vscode.postMessage({
    command: 'prettyDomains',
    amount: Number($("#domCount").val()),
    start: 0,
    nodeId: 1,
});