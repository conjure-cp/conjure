// module.exports = function (n) { return n * 0 }

module.exports.bindKeys = () => {
    Mousetrap.bind('n', next, 'keydown')
    Mousetrap.bind('p', previous, 'keydown')
    Mousetrap.bind('r', goRight, 'keydown')
    Mousetrap.bind('l', goLeft, 'keydown')
    Mousetrap.bind('u', goUp, 'keydown')
}

module.exports.next = () => {
    selectedNode = (selectedNode + 1) % (Object.keys(treeviewDomainMap).length + 1);
    if (selectedNode == 0) {
        selectedNode++;
    }
    selectNode(selectedNode);
    focusNode(nodeMap[selectedNode]);
}

module.exports.previous = () => {
    selectedNode = (selectedNode - 1) % (Object.keys(treeviewDomainMap).length + 1);
    if (selectedNode == 0) {
        selectedNode = Object.keys(treeviewDomainMap).length;
    }
    selectNode(selectedNode);
    focusNode(nodeMap[selectedNode]);
}

// module.exports.goUp = (parentMap, selectedNode) => {

//     let nextNode;

//     if (selectedNode in parentMap) {
//         nextNode = parentMap[selectedNode];
//     }
//     else {
//         for (const key in nodeMap) {
//             if (nodeMap[key].children) {
//                 if (nodeMap[key].children.includes(nodeMap[selectedNode])) {
//                     nextNode = nodeMap[key];
//                     break;
//                 }
//             }
//         }
//     }

//     parentMap[selectedNode] = nextNode;
//     selectedNode = nextNode.minionID;

//     selectNode(selectedNode);
//     focusNode(nodeMap[selectedNode]);
// }

// module.exports.goLeft = () => {
//     if (nodeMap[selectedNode].children.length > 0) {
//         selectedNode = nodeMap[selectedNode].children[0].minionID;
//         selectNode(selectedNode);
//         focusNode(nodeMap[selectedNode]);
//     }
// }

// module.exports.goRight = () => {
//     if (nodeMap[selectedNode].children.length == 2) {
//         selectedNode = nodeMap[selectedNode].children[1].minionID;
//         selectNode(selectedNode);
//         focusNode(nodeMap[selectedNode]);
//     }
// }
