import 'mocha';
import * as chai from 'chai';

import Node from '../src/util/Node';
import Globals from '../src/util/Globals';
import Data from '../src/util/Data';




describe('AddNode', () => {

  var d: Data;

  beforeEach(() => {
    d = new Data();
    // ...
  });

  it('should add the first node to the map', () => {
    d.addNode(0, -1, "");
    chai.assert((d.id2Node[0].id) === 0);
    chai.assert((d.id2Node[0].name) === "");
    chai.assert((d.id2Node[0].parent) === undefined);
    chai.assert((d.id2Node[0].children) === null);
    chai.assert((d.id2Node[0]._children) === null);
    chai.assert((d.id2Node[0].x) === 0);
    chai.assert((d.id2Node[0].y) === 0);

    chai.assert.equal(d.totalLoaded, 1);
  });

  it('should add the second node to the map as a child', () => {
    d.addNode(0, -1, "");
    d.addNode(1, 0, "child");
    chai.assert((d.id2Node[0].children[0]) === d.id2Node[1]);
    chai.assert((d.id2Node[0]._children) === null);

    chai.assert((d.id2Node[1].id) === 1);
    chai.assert((d.id2Node[1].name) === "child");
    chai.assert((d.id2Node[1].parent) === d.id2Node[0]);
    chai.assert((d.id2Node[1].children) === null);
    chai.assert((d.id2Node[1]._children) === null);
    chai.assert((d.id2Node[1].x) === 0);
    chai.assert((d.id2Node[1].y) === 0);

    chai.assert.equal(d.totalLoaded, 2);
  });
});

describe('collapseNode', () => {
  it('Should collapse all children for all decendants', () => {
    var tree = new Data();
    tree.addNode(0, -1, "");
    tree.addNode(1, 0, "");
    tree.addNode(2, 0, "");
    tree.addNode(3, 1, "");
    tree.addNode(4, 3, "");
    tree.addNode(5, 2, "");
    tree.hideChildren(0);
    console.log(tree.id2Node[0]);

  })



});

// // suite("Extension Tests", function () {

// //     test("Parse DB", () => {
// //         console.log("DSADASDASDAS");
// //         console.log(new Node(1, "asda", null));
// //         });
// // });