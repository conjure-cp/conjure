import Listview from '../src/modules/Listview';
import State from '../src/modules/State';
import { lstat } from 'fs';

declare var d3: any;

describe('Test the Listview class', function () {

    let ls: Listview;

    beforeEach(function () {
        ls = new Listview();

        State.id2Node = {};
        State.solAncestorIds = [];
        State.solNodIds = [];
        State.rootId = 0;
        State.selectedId = State.rootId;
        State.waiting = false;
        State.pretty = true;
        State.totalLoaded = 0;

        $("body").empty();

    });

    describe('Tabulate', function () {

        it('Should create a table header', function () {
            d3.select("body")
                .append("div")
                .attr("id", "pane");



            ls.tabulate();

            // console.log($("#pane table thead tr th").text());
            expect($("#pane table thead tr th").text()).toBe("NameDomain");
        });
    });

    describe('Append rows', function () {

        it('Should append rows to header', function () {
            d3.select("body")
                .append("div")
                .attr("id", "pane");



            ls.tabulate();

            let data = [{ name: "x", rng: "int(1..5)" }, { name: "y", rng: "int(0..1)" }];
            ls.appendRows(data);

            // console.log($("#pane table tbody").text());
            expect($("#pane table tbody").text()).toBe("xint(1..5)yint(0..1)");
        });
    });

    describe('create ul', function () {

        it('Should create a ul in the pane', function () {
            d3.select("body")
                .append("div")
                .attr("id", "pane");

            ls.createUL();

            // console.log($("#pane"));

            expect($("#pane ul")).not.toBeNull();

        });
    });

    describe('get set path', function () {

        it('Should return the path to the inner set', function () {
            
            let container: any = {
                name: "Domain Variables",
            };

            let outerSet: any = {
                name: "s",
                parent: container
            };

            let innerSet: any = {
                name: "child",
                parent: outerSet
            };

            expect(ls.getSetPath(innerSet)).toBe("s.child");

        });
    });

    describe('getVarname', function () {

        it('Should just return the variable name', function () {

            // let parent = new Node(0, "simple", "pretty", null, 100, true, 2, false);
            let container: any = {
                name: "Domain Variables",
            };

            let outerSet: any = {
                name: "s",
                parent: container
            };

            let innerSet: any = {
                name: "child",
                parent: outerSet
            };

            innerSet["children"] = [{name: "Cardinality"}];
            outerSet["children"] = [{innerSet}, {name:"Cardinality"}];

            expect(Listview.getVarName(innerSet)).toBe("schild");
        });
    });

    describe('setChanged', function () {

        it('should set the class of changed variables to "changed"', function () {
            let pane = d3.select("body")
                .append("div")
                .attr("id", "pane");

            pane.append("li")
                 .attr("id", "s");

            pane.append("li")
                 .attr("id", "child");


            let outerSet: any = {
                name: "s",
            };

            let innerSet: any = {
                name: "child",
                parent: outerSet
            };

            ls.id2Node["child"] = innerSet;

            let changedList = ["child"];

            ls.setChanged(changedList);

            // console.log($("#child"));
            // console.log($("#pane"));

            expect($("#child").attr("class")).toBe("changed");
            expect($("#s").attr("class")).toBe("changed");
        });
    });
});

