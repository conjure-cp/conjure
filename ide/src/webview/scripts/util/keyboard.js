// module.exports.bindKeys = () => {
//     Mousetrap.bind('n', next, 'keydown')
//     // Mousetrap.bind('p', previous, 'keydown')
//     // Mousetrap.bind('r', goRight, 'keydown')
//     // Mousetrap.bind('l', goLeft, 'keydown')
//     // Mousetrap.bind('u', goUp, 'keydown')
// }

// module.exports.next = () => {
//     selectedNode = (selectedNode + 1) % (Object.keys(treeviewDomainMap).length + 1);
//     if (selectedNode == 0) {
//         selectedNode++;
//     }
//     selectNode(selectedNode);
// }

import Mousetrap from "./mousetrap"


export function bindKeys(loadNNodes){

Mousetrap.bind('n', loadNNodes, 'keydown');

}