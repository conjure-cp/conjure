exports.vscode = acquireVsCodeApi();
exports.currentId = 1;
exports.nodeMap = {};
exports.root = {}
// nodeMap[-1] = {id: -1};

exports.viewerWidth = $(document).width();
exports.viewerHeight = $(document).height();
exports.margin = { top: 40, right: 30, bottom: 50, left: 30 },
    exports.width = exports.viewerWidth - exports.margin.left - exports.margin.right;
exports.height = exports.viewerHeight - exports.margin.top - exports.margin.bottom;

exports.i = 0;
exports.duration = 750;
exports.tree = d3.layout.tree().size([exports.height, exports.width]);

exports.waiting = false;

exports.diagonal = d3.svg.diagonal()
    .projection((d) => {
        return [d.x, d.y];

    });

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

exports.addNode = (parentId) => {

    exports.currentId++;
    let newNode = {id: exports.currentId};

    // console.log(exports.currentId);
    // console.log(parentId);
    // console.log(exports.nodeMap);

    if (!exports.nodeMap[parentId].children) {
        exports.nodeMap[parentId].children = [];
    }

    exports.nodeMap[parentId].children.push(newNode);
    exports.nodeMap[exports.currentId] = newNode;

}

exports.getChildren = (parentId) => {
    exports.vscode.postMessage({
        command: 'children',
        parentId: parentId,
    });
}

exports.setHasMore = (nodeId) => {
    let s = "#node" + nodeId + " circle";
    $(s).attr("class", "hasOthers")
}

exports.unsetHasMore = (nodeId) => {
    let s = "#node" + nodeId + " circle";
    $(s).attr("class", "")
}

exports.vscode.postMessage({
    command: 'init',
});