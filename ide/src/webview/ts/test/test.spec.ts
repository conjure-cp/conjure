// import 'mocha';
// import * as chai from 'chai';

// import Node from '../src/util/Node';
// import Globals from '../src/util/Globals';
// import Data from '../src/util/Data';


// let makeData = () => {
//     var tree = new Data();
//     tree.addNode(0, -1, "");
//     tree.id2ChildIds[0] = [1, 2];
//     tree.addNode(1, 0, "");
//     tree.id2ChildIds[1] = [3];
//     tree.addNode(2, 0, "");
//     tree.id2ChildIds[2] = [5];
//     tree.addNode(3, 1, "");
//     tree.id2ChildIds[3] = [4];
//     tree.addNode(4, 3, "");
//     tree.id2ChildIds[4] = null;
//     tree.id2ChildIds[5] = null;
//     tree.addNode(5, 2, "");
//     tree.correctPath = [0, 2, 5];
//     return tree;
// }



// describe('AddNode', () => {

//   var d: Data;

//   beforeEach(() => {
//     d = new Data();
//     // ...
//   });

//   it('should add the first node to the map', () => {
//     d.addNode(0, -1, "");
//     chai.assert((d.id2Node[0].id) === 0);
//     chai.assert((d.id2Node[0].name) === "");
//     chai.assert((d.id2Node[0].parent) === undefined);
//     chai.assert((d.id2Node[0].children) === null);
//     chai.assert((d.id2Node[0]._children) === null);
//     chai.assert((d.id2Node[0].x) === 0);
//     chai.assert((d.id2Node[0].y) === 0);

//     chai.assert.equal(d.totalLoaded, 1);
//   });

//   it('should add the second node to the map as a child', () => {
//     d.addNode(0, -1, "");
//     d.addNode(1, 0, "child");
//     chai.assert((d.id2Node[0].children[0]) === d.id2Node[1]);
//     chai.assert((d.id2Node[0]._children) === null);

//     chai.assert((d.id2Node[1].id) === 1);
//     chai.assert((d.id2Node[1].name) === "child");
//     chai.assert((d.id2Node[1].parent) === d.id2Node[0]);
//     chai.assert((d.id2Node[1].children) === null);
//     chai.assert((d.id2Node[1]._children) === null);
//     chai.assert((d.id2Node[1].x) === 0);
//     chai.assert((d.id2Node[1].y) === 0);

//     chai.assert.equal(d.totalLoaded, 2);
//   });
// });

// describe('collapseNode', () => {
//   var data: Data;

//   beforeEach(() => {
//     data = makeData();
//   });

//   it('Should collapse all children for all decendants', () => {
//     // tree.hideChildren(0);
//     data.collapseNode(0);
//     // console.log(tree.id2Node[0]);
//     for (let i = 0; i < 5; i++) {
//       // console.log("-----------");
//       // console.log(tree.id2Node[i]);
//       chai.assert.isNull(data.id2Node[i].children);
//     }
//   });

//   it('Should expand all children for all decendants', () => {
//     data.collapseNode(0);
//     data.expandNode(0);
//     for (let i = 0; i < 5; i++) {
//       chai.assert.isNull(data.id2Node[i]._children);
//     }
//   });
// });


// describe('toggleNode', () => {
//   var data: Data;

//   beforeEach(() => {
//     data = makeData();
//   });

//   it('Toggling twice does nothing', () => {
//     data.toggleNode(0);
//     chai.assert.equal(data.id2Node[0].children, null);
//     data.toggleNode(0);
//     chai.assert.equal(data.id2Node[0]._children, null);
//   });

// });
// // // suite("Extension Tests", function () {

// describe('collapseFailed', () => {
//   var data: Data;

//   beforeEach(() => {
//     data = makeData();
//   });

//   it('collapsing failed', () => {
//     data.collapseFailed();
//     chai.assert.isNull(data.id2Node[1].children);

//   });

// });
// // //     test("Parse DB", () => {
// // //         console.log("DSADASDASDAS");
// // //         console.log(new Node(1, "asda", null));
// // //         });
// // // });