exports.vscode = acquireVsCodeApi();
exports.currentId = 1;
exports.selectedId = 1;
exports.id2Node = {};
exports.id2Parent = {};
exports.root = { id: 1 };

exports.viewerWidth = $(document).width();
exports.viewerHeight = $(document).height();
exports.margin = { top: 40, right: 30, bottom: 50, left: 30 };
exports.width = exports.viewerWidth - exports.margin.left - exports.margin.right;
exports.height = exports.viewerHeight - exports.margin.top - exports.margin.bottom;

exports.i = 0;
exports.duration = 750;
exports.tree = d3.layout.tree().size([exports.height, exports.width]);

exports.waiting = false;

let z = -1;

exports.setup = (zoom) => {
    z = zoom;
}

exports.diagonal = d3.svg.diagonal()
    .projection((d) => {
        return [d.x, d.y];

    });

exports.toggleNode = (nodeId) => {
    if (exports.id2Node[nodeId]._children) {

        exports.id2Node[nodeId].children = exports.id2Node[nodeId]._children;
        exports.id2Node[nodeId]._children = null;
        exports.unsetHasOthers(nodeId);
    }
    else {
        exports.id2Node[nodeId]._children = exports.id2Node[nodeId].children;
        exports.id2Node[nodeId].children = null;
        exports.setHasOthers(nodeId);
        
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
    // console.log(exports.selectedId + 1);
    // console.log(exports.id2Node);
    // console.log(exports.id2Node[exports.selectedId+1]);
    if (!exports.id2Node[exports.selectedId + 1]) {
        exports.loadNNodes();
    }
    else {
        exports.selectedId++;
        exports.selectNode(exports.selectedId);
    }
}

exports.previousNode = () => {
    if (exports.selectedId > 1) {
        exports.selectedId = exports.id2Parent[exports.selectedId];
    }
    exports.selectNode(exports.selectedId);
}

exports.loadNNodes = () => {
    if (!exports.waiting) {
        exports.vscode.postMessage({
            command: 'nParents',
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
}

exports.addNode = (parentId) => {

    exports.currentId++;
    let newNode = { id: exports.currentId };

    // console.log(exports.currentId);
    // console.log(parentId);
    // console.log(exports.id2Node);

    if (!exports.id2Node[parentId].children) {
        exports.id2Node[parentId].children = [];
    }

    exports.id2Node[parentId].children.push(newNode);
    exports.id2Node[exports.currentId] = newNode;
    exports.id2Parent[exports.currentId] = parentId;

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
    // $(s).attr("class", "hasOthers")
}

exports.unsetHasOthers = (nodeId) => {
    let s = "#node" + nodeId + " circle";
    d3.select(s).classed("hasOthers", false);
}

exports.vscode.postMessage({
    command: 'init',
});