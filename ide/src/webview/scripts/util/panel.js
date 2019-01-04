// import colours from './colours.js';
// import globals from './globals.js';



// let allTreeViewNodes;

// export default function showPretty(json) {

//     let expandedNodes;
//     let t = $('#pane');

//     if (!globals.init) {
//         expandedNodes = t.treeview('getExpanded');
//     }

//     t.treeview({
//         expandIcon: "glyphicon glyphicon-triangle-right",
//         collapseIcon: "glyphicon glyphicon-triangle-bottom",
//         color: "white",
//         backColor: colours.bgColour,
//         onhoverColor: "purple",
//         borderColor: colours.bgColour,
//         showBorder: false,
//         showTags: true,
//         highlightSelected: true,
//         multiSelect: true,
//         selectedColor: "black",
//         selectedBackColor: "coral",
//         data: json,
//         onNodeSelected: function (event, node) {
//             t.treeview('unselectNode', [node.nodeId, { silent: true }]);
//             selectChanged();
//             // console.log(node);
//         },
//         // onSearchComplete: (event, node) => {
//         //     // console.log(node);
//         // }
//     });

//     if (globals.init) {
//         t.treeview('expandAll', { levels: 10000000001, silent: true });
//         expandedNodes = t.treeview('getExpanded');
//         allTreeViewNodes = expandedNodes;
//         globals.init = false;
//     }

//     // expandedNodes.forEach(element => {
//     //     $('#pane').treeview('expandNode', [element.nodeId, { levels: 1, silent: true }]);
//     // })

//     let blah = t.treeview('getNode', 1);
//     t.treeview('getNode', 1).text = "DASDASDASD";
//     console.log(blah);
// }



// function showDomains(nodeId) {

//     let checked = (document.getElementById('check').checked);
//     panel.setHeaderTitle("Status at Node: " + nodeId);

//     if (checked) {

//         let setAttributeList = ["Cardinality", "Excluded", "Included"];

//         function unselectAll() {
//             let selected = t.treeview('getSelected');
//             selected.forEach(element => {
//                 t.treeview('unselectNode', [element, { silent: true }]);
//             });
//         }

//         function selectVariable(diff, m) {

//             if (setAttributeList.includes(m.text)) {
//                 t.treeview('selectNode', [m.nodeId, { silent: true }]);
//             }

//             if (m.text in normalDomainMap[nodeId]) {

//                 let variable = normalDomainMap[nodeId][m.text];

//                 if (!("table" in variable)) {
//                     t.treeview('selectNode', [m.nodeId, { silent: true }]);
//                 }
//             }

//             if (!(typeof diff === 'string')) {

//                 for (const key in diff) {
//                     if (key != "_t") {
//                         selectVariable(diff[key], m[key]);
//                     }
//                 }
//             }
//         }

//         function selectChanged() {

//             unselectAll();

//             if (nodeId != 1) {

//                 let delta = jsondiffpatch.diff(treeviewDomainMap[nodeId - 1], treeviewDomainMap[nodeId]);

//                 if (delta) {
//                     selectVariable(delta, allTreeViewNodes);
//                 }
//             }
//         }

//         let expandedNodes;
//         let t = $('#pane');


//         if (!init) {
//             expandedNodes = t.treeview('getExpanded');
//         }

//         $("#pane").empty();

//         $('#pane').treeview({
//             expandIcon: "glyphicon glyphicon-triangle-right",
//             collapseIcon: "glyphicon glyphicon-triangle-bottom",
//             color: "white",
//             backColor: colours.bgColour,
//             onhoverColor: "purple",
//             borderColor: colours.bgColour,
//             showBorder: false,
//             showTags: true,
//             highlightSelected: true,
//             multiSelect: true,
//             selectedColor: "black",
//             selectedBackColor: "coral",
//             data: treeviewDomainMap[nodeId],
//             onNodeSelected: function (event, node) {
//                 t.treeview('unselectNode', [node.nodeId, { silent: true }]);
//                 selectChanged();
//                 // console.log(node);
//             },
//             // onSearchComplete: (event, node) => {
//             //     // console.log(node);
//             // }
//         });

//         if (init) {
//             t.treeview('expandAll', { levels: 10000000001, silent: true });
//             expandedNodes = t.treeview('getExpanded');
//             allTreeViewNodes = expandedNodes;
//             init = false;
//         }

//         expandedNodes.forEach(element => {
//             $('#pane').treeview('expandNode', [element.nodeId, { levels: 1, silent: true }]);
//         })

//         selectChanged();
//     }
//     else {
//         $("#pane").empty();
//         tabulate(simpleDomainMap[nodeId], ['name', 'range']);

//         if (nodeId != 1) {
//             let delta = jsondiffpatch.diff(simpleDomainMap[nodeId - 1], simpleDomainMap[nodeId]);
//             for (const key in delta) {
//                 if (key != "_t") {
//                     let changedVar = simpleDomainMap[nodeId][Number(key)];
//                     $("#row" + key).toggleClass("changed");
//                 }
//             }
//         }

//     }
// }
