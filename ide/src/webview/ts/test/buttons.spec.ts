import State from '../src/modules/State';
import Node from '../src/modules/Node';
import Buttons from '../src/controls/Buttons';

declare var d3: any;

describe('Test the Buttons class', function () {

    beforeEach(function () {
        State.id2Node = {};
        State.id2Node[0] = new Node(0, "simple", "pretty", null, 100, true, 2, false);
        State.solAncestorIds = [];
        State.solNodIds = [];
        State.rootId = 0;
        State.selectedId = State.rootId;
        State.waiting = false;
        State.pretty = true;
        State.totalLoaded = 0;

        $("body").empty();

    });

    // describe('Pretty checkbox handler ', function () {

    //     it('Should set pretty to false', function () {
    //         Buttons.handlePrettyCheckBox();
    //         expect(State.pretty).toBeFalsy();
    //     });

    //     it('Should set pretty to true', function () {
    //         State.pretty = false;
    //         Buttons.handlePrettyCheckBox();
    //         expect(State.pretty).toBeTruthy();
    //     });
    // });

    describe('Label handler ', function () {

        it('Should set the label to pretty', function () {

            d3.select("body")
                .append("input")
                .attr("checked", true)
                .attr("id", "labels");

            d3.select("body")
                .append("svg")
                .append("g")
                .attr("class", "node")
                .attr("id", "node1")
                .append("text");

            // expect(fail);

            let node1 = new Node(1, "simple", "pretty", null, 100, true, 2, false);

            State.id2Node[1] = node1;

            $("#node1 text").text("blah");

            Buttons.handleLabels();

            expect($("#node1 text").text()).toBe("pretty");
        });

        it('Should set the label to simple', function () {

            d3.select("body")
                .append("input")
                .attr("id", "labels");

            d3.select("body")
                .append("svg")
                .append("g")
                .attr("class", "node")
                .attr("id", "node1")
                .append("text");

            // expect(fail);

            let node1 = new Node(1, "simple", "pretty", null, 100, true, 2, false);

            State.id2Node[1] = node1;

            $("#node1 text").text("blah");

            Buttons.handleLabels();

            expect($("#node1 text").text()).toBe("simple");
        });
    });
});