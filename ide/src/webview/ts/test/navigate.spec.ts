import State from '../src/modules/State';
import Node from '../src/modules/Node';

import * as sinon from "sinon";
import Navigate from '../src/modules/Navigate';

declare var d3: any;

var vscodeApi: any = {
    postMessage: function (message: any) {
        return;
    }
};

var spy: any = sinon.spy(vscodeApi, "postMessage");
let loadNodeMessage = { command: 'loadNodes', amount: 1, start: 0 };

describe('Test the Navigate class', function () {

    beforeEach(function () {
        State.id2Node = {};
        State.solAncestorIds = [];
        State.solNodIds = [];
        State.rootId = 0;
        State.selectedId = State.rootId;
        State.waiting = false;
        State.pretty = true;

        $("body").empty();

        d3.select("body")
            .append("input")
            .attr("checked", true)
            .attr("id", "expressions");
    });

    afterEach(function () {
        spy.resetHistory();
    });

    describe('Next node ', function () {

        it('Should not increment the selectedId', function () {

            let node = new Node(0, "simple", "pretty", null, 2, true, 2, false);

            State.addNode(-1, node);

            Navigate.nextNode(vscodeApi);

            expect(State.waiting);
            expect(State.selectedId).toBe(0);

            sinon.assert.called(spy);
            sinon.assert.calledWith(spy, loadNodeMessage);
        });

        it('Should increment the selectedId', function () {

            let node = new Node(0, "simple", "pretty", null, 2, true, 2, false);
            let child = new Node(1, "simple", "pretty", null, 0, true, 0, false);

            node.children = [child];

            State.addNode(-1, node);
            State.addNode(0, child);

            Navigate.nextNode(vscodeApi);

            expect(State.selectedId).toBe(1);

            sinon.assert.notCalled(spy);
        });

        it('Should Toggle the hidden child', function () {

            let node = new Node(0, "simple", "pretty", null, 2, true, 2, false);
            let child = new Node(1, "simple", "pretty", null, 0, true, 0, false);

            node._children = [child];

            State.addNode(-1, node);

            Navigate.nextNode(vscodeApi);

            expect(node._children).toBeNull();
            expect(node.children).toEqual([child]);

            sinon.assert.notCalled(spy);
        });
    });

    describe('Previous node', function () {

        it('Should decrement the selectedId', function () {

            let node = new Node(0, "simple", "pretty", null, 2, true, 2, false);
            let child = new Node(1, "simple", "pretty", null, 0, true, 0, false);

            State.addNode(-1, node);
            State.addNode(0, child);
            State.selectedId = 1;

            Navigate.previousNode();

            expect(State.selectedId).toBe(0);
        });

        it('Should not decrement the selectedId', function () {

            let node = new Node(0, "simple", "pretty", null, 2, true, 2, false);
            State.addNode(-1, node);

            Navigate.previousNode();

            expect(State.selectedId).toBe(0);
        });
    });

    describe('Right node', function () {

        it('Should update the selectedId', function () {

            let node = new Node(0, "simple", "pretty", null, 2, true, 2, false);
            let childLeft = new Node(1, "simple", "pretty", null, 0, true, 0, false);
            let childRight = new Node(2, "simple", "pretty", null, 0, false, 0, false);

            State.addNode(-1, node);
            State.addNode(0, childLeft);
            State.addNode(0, childRight);

            Navigate.rightNode();

            expect(State.selectedId).toBe(2);
        });

        it('Should not update the selectedId', function () {

            let node = new Node(0, "simple", "pretty", null, 2, true, 2, false);
            let childLeft = new Node(1, "simple", "pretty", null, 0, true, 0, false);

            State.addNode(-1, node);
            State.addNode(0, childLeft);

            Navigate.rightNode();

            expect(State.selectedId).toBe(0);
        });
    });

    describe('Left node', function () {

        it('Should update the selectedId', function () {

            let node = new Node(0, "simple", "pretty", null, 2, true, 2, false);
            let childLeft = new Node(1, "simple", "pretty", null, 0, true, 0, false);
            let childRight = new Node(2, "simple", "pretty", null, 0, false, 0, false);

            State.addNode(-1, node);
            State.addNode(0, childLeft);
            State.addNode(0, childRight);

            Navigate.leftNode();

            expect(State.selectedId).toBe(1);
        });

        it('Should not update the selectedId', function () {

            let node = new Node(0, "simple", "pretty", null, 2, true, 2, false);
            let childLeft = new Node(1, "simple", "pretty", null, 0, true, 0, false);

            State.addNode(-1, node);
            State.addNode(0, childLeft);

            Navigate.leftNode();

            expect(State.selectedId).toBe(0);
        });
    });

    describe('Up node', function () {

        it('Should update the selectedId', function () {

            let node = new Node(0, "simple", "pretty", null, 2, true, 2, false);
            let childLeft = new Node(1, "simple", "pretty", node, 0, true, 0, false);

            State.addNode(-1, node);
            State.addNode(0, childLeft);

            State.selectedId = 1;

            Navigate.upNode();

            expect(State.selectedId).toBe(0);
        });

        it('Should not update the selectedId', function () {

            let node = new Node(0, "simple", "pretty", null, 2, true, 2, false);
            State.addNode(-1, node);

            Navigate.upNode();
            expect(State.selectedId).toBe(0);
        });
    });

    describe('Next solution node', function () {

        it('Should not update the selectedId', function () {

            Navigate.nextSolutionNode();
            expect(State.selectedId).toBe(0);
        });

        it('Should go to the first solution node', function () {

            let randomNode = new Node(0, "simple", "pretty", null, 2, true, 2, false);
            let solNode1 = new Node(4, "simple", "pretty", null, 0, true, 0, true);
            let solNode2 = new Node(10, "simple", "pretty", null, 0, true, 0, true);

            State.addNode(-1, randomNode);
            State.addNode(0, solNode1);
            State.addNode(0, solNode2);

            State.solNodIds = [solNode1.id, solNode2.id];

            Navigate.nextSolutionNode();
            expect(State.selectedId).toBe(solNode1.id);
        });

        it('Should go to the second solution node', function () {

            let randomNode = new Node(0, "simple", "pretty", null, 2, true, 2, false);
            let solNode1 = new Node(4, "simple", "pretty", null, 0, true, 0, true);
            let solNode2 = new Node(10, "simple", "pretty", null, 0, true, 0, true);

            State.addNode(-1, randomNode);
            State.addNode(0, solNode1);
            State.addNode(0, solNode2);

            State.solNodIds = [solNode1.id, solNode2.id];

            Navigate.nextSolutionNode();
            Navigate.nextSolutionNode();
            expect(State.selectedId).toBe(solNode2.id);
        });
    });

    describe('Previous solution node', function () {

        it('Should not update the selectedId', function () {
            Navigate.previousSolutionNode();
            expect(State.selectedId).toBe(0);
        });

        it('Should go to the first solution node', function () {

            let randomNode = new Node(0, "simple", "pretty", null, 2, true, 2, false);
            let solNode1 = new Node(4, "simple", "pretty", null, 0, true, 0, true);
            let solNode2 = new Node(10, "simple", "pretty", null, 0, true, 0, true);

            State.addNode(-1, randomNode);
            State.addNode(0, solNode1);
            State.addNode(0, solNode2);

            State.solNodIds = [solNode1.id, solNode2.id];

            Navigate.previousSolutionNode();
            expect(State.selectedId).toBe(solNode2.id);
        });

        it('Should go to the second solution node', function () {

            let randomNode = new Node(0, "simple", "pretty", null, 2, true, 2, false);
            let solNode1 = new Node(4, "simple", "pretty", null, 0, true, 0, true);
            let solNode2 = new Node(10, "simple", "pretty", null, 0, true, 0, true);

            State.addNode(-1, randomNode);
            State.addNode(0, solNode1);
            State.addNode(0, solNode2);

            State.solNodIds = [solNode1.id, solNode2.id];

            Navigate.previousSolutionNode();
            Navigate.previousSolutionNode();
            expect(State.selectedId).toBe(solNode1.id);
        });
    });
});