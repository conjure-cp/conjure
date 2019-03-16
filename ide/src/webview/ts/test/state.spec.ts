import State from '../src/testable/State';
import Node from '../src/testable/Node';

declare var d3: any;

describe('Test the State class ', function () {
    beforeEach(function() {
        State.id2Node = {};
        State.solAncestorIds = [];
        State.solNodIds = [];
        $("body").empty();
    });

    function appendCircleToDom(){
            d3.select("body")
                .append("svg")
                    .append("g")
                    .attr("id", "node1")
                        .append("circle")
                            .attr("r", 20);
    }

    describe('Add the root node', function () {
        it('should be place in the map', function () {
            let node = new Node(0, "simple", "pretty", null, 2, true, 2, false);
            State.addNode(-1, node);
            expect(State.id2Node[0]).toBe(node);
        });
    });


    describe('Add child node', function () {
        it('should be placed in the map', function () {
            let parent = new Node(0, "simple", "pretty", null, 2, true, 2, false);
            State.addNode(-1, parent);
            expect(State.id2Node[0]).toBe(parent);

            let child1 = new Node(1, "s", "p", parent, 0, true, 0, false);
            State.addNode(0, child1);

            expect(State.id2Node[1]).toBe(child1);
            expect(State.id2Node[0].children).toEqual([child1]);
            expect(State.id2Node[1].parent).toEqual(parent);
        });
    });

    describe('Add left child then right child', function () {
        it('should be on the correct branching side', function () {
            let parent = new Node(0, "simple", "pretty", null, 2, true, 2, false);
            // expect(State.id2Node[0]).toBe(parent);

            let child1 = new Node(1, "s", "p", parent, 0, true, 0, false);
            let child2 = new Node(2, "s", "p", parent, 0, false, 0, false);
            State.addNode(-1, parent);
            State.addNode(0, child1);
            State.addNode(0, child2);

            expect(State.id2Node[0].children).toEqual([child1, child2]);
        });
    });

    describe('Add right child then left child', function () {
        it('should be on the correct branching side', function () {
            let parent = new Node(0, "simple", "pretty", null, 2, true, 2, false);
            // expect(State.id2Node[0]).toBe(parent);

            let child1 = new Node(1, "s", "p", parent, 0, false, 0, false);
            let child2 = new Node(2, "s", "p", parent, 0, true, 0, false);
            State.addNode(-1, parent);
            State.addNode(0, child1);
            State.addNode(0, child2);

            expect(State.id2Node[0].children).toEqual([child2, child1]);
        });
    });

    // describe('Collapse failed branches', function () {
    //     it('Failed branches should be collapsed', function () {
    //         let parent = new Node(0, "simple", "pretty", null, 3, true, 2, false);
    //         let solutionChild = new Node(1, "s", "p", parent, 0, true, 0, true);

    //         let failedChild = new Node(2, "s", "p", parent, 1, false, 1, false);
    //         let failedGrandChild = new Node(3, "s", "p", failedChild, 0, false, 0, false);

    //         State.solAncestorIds = [parent.id, solutionChild.id];

    //         State.addNode(-1, parent);
    //         State.addNode(0, solutionChild);
    //         State.addNode(0, failedChild);
    //         State.addNode(2, failedGrandChild);

    //         State.collapseFailed();

    //         expect(State.id2Node[2].children).toBeNull();

    //     });
    // });

    describe('Fill circle', function () {
        it('Should append the solution class to solution nodes', function () {
            let parent = new Node(0, "simple", "pretty", null, 3, true, 2, false);
            let solutionChild = new Node(1, "s", "p", parent, 0, true, 0, true);
            State.addNode(-1, parent);
            State.addNode(0, solutionChild);

            appendCircleToDom();

            State.fillCircle(solutionChild);
            expect($("#node1 circle").attr("class")).toBe("solution");
        });
    });

    describe('Fill circle', function () {
        it('Should set the class of nodes with others that are on failed branches to hasmore red' , function () {
            let parent = new Node(0, "simple", "pretty", null, 3, true, 2, false);
            let failedChildThatHasMoreNodes = new Node(1, "s", "p", parent, 2, true, 2, false);
            State.addNode(-1, parent);
            State.addNode(0, failedChildThatHasMoreNodes);

            appendCircleToDom();

            State.fillCircle(failedChildThatHasMoreNodes);
            // console.log($("#node1 circle").attr("class"));
            expect($("#node1 circle").attr("class")).toBe("hasOthers red");
        });
    });

    describe('Fill circle', function () {
        it('Should set the class of nodes with others that are on sol branches to hasmore' , function () {
            let parent = new Node(0, "simple", "pretty", null, 3, true, 2, false);
            let childThatHasMoreNodes = new Node(1, "s", "p", parent, 2, true, 2, false);
            let grandChildSolNode = new Node(2, "s", "p", parent, 0, true, 0, true);
            State.addNode(-1, parent);
            State.addNode(0, childThatHasMoreNodes);
            State.addNode(1, grandChildSolNode);

            State.solNodIds = [grandChildSolNode.id];
            State.solAncestorIds = [parent.id, childThatHasMoreNodes.id];

            appendCircleToDom();

            State.fillCircle(childThatHasMoreNodes);
            // console.log($("#node1 circle").attr("class"));
            expect($("#node1 circle").attr("class")).toBe("hasOthers");
        });
    });

});
