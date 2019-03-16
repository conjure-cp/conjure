import Tree from '../src/modules/Tree';
import State from '../src/modules/State';
import Node from '../src/modules/Node';

declare var d3: any;

describe('Test the Tree class', function () {

    beforeEach(function () {
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


    describe('makeSvg ', function () {

        it('Should ad an svg container to the dom', function () {
            Tree.makeSvg("body", () => { }, 100, 500);

            var body = d3.select("body");
            expect(body.select("svg")).not.toBeNull();
            expect(body.select("svg").select("g")).not.toBeNull();
            expect($("#theTree").width()).toBe(100);
            expect($("#theTree").height()).toBe(500);

        });
    });

    describe('selectNode ', function () {

        it('Should change selected node', function () {

            let parent = new Node(0, "simple", "pretty", null, 2, true, 2, false);
            let child = new Node(1, "simple", "pretty", parent, 0, true, 0, false);

            State.addNode(-1, parent);
            State.addNode(0, child);

            var body = d3.select("body");

            body.append("g")
                .attr("class", "node")
                .attr("id", "node0")
                .append("circle")
                .attr("class", "selected");

            body.append("g")
                .attr("class", "node")
                .attr("id", "node1")
                .append("circle");


            Tree.selectNode(1);

            expect($("#node0 circle").attr("class")).not.toBe("selected");
            expect($("#node1 circle").attr("class")).toBe("selected");
        });
    });

    describe('appendNodeGroup ', function () {

        it('Should append a <g> for each node in the tree ', function () {

            var svg = d3.select("body")
                .append("svg")
                .append("g");

            let parent = new Node(0, "simple", "pretty", null, 2, true, 2, false);
            let child = new Node(1, "simple", "pretty", parent, 0, true, 0, false);

            parent.children = [child];

            let nodeList = Tree.getNodeList(parent);

            let domObjects = svg.selectAll("g.node")
                .data(nodeList, (node: Node) => { return node.id; });

            Tree.enterNodes(domObjects);

            expect($("#node0")).not.toBeNull();
            expect($("#node0").attr("class")).toBe("node");
            expect($("#node1")).not.toBeNull();
            expect($("#node1").attr("class")).toBe("node");

        });
    });

    describe('appendCircle ', function () {

        it('Should append an svg circle to the selection', function () {

            var svg = d3.select("body")
                .append("svg")
                .append("g");

            let parent = new Node(0, "simple", "pretty", null, 2, true, 2, false);
            let child = new Node(1, "simple", "pretty", parent, 0, true, 0, false);

            parent.children = [child];

            let nodeList = Tree.getNodeList(parent);

            let domObjects = svg.selectAll("g.node")
                .data(nodeList, (node: Node) => { return node.id; });

            let nodeEnter = Tree.enterNodes(domObjects);

            Tree.appendCircle(nodeEnter);

            expect($("#node0 circle")).not.toBeNull();
            expect($("#node1 circle")).not.toBeNull();

        });
    });

    describe('append label text ', function () {
        it('Should append the pretty label text', function () {

            var svg = d3.select("body")
                .append("svg")
                .append("g");

            d3.select("body")
                .append("input")
                .attr("type", "checkbox")
                .attr("checked", true)
                .attr("id", "labels");

            let parent = new Node(0, "simple", "pretty", null, 2, true, 2, false);
            let child = new Node(1, "simple", "pretty", parent, 0, true, 0, false);

            parent.children = [child];

            let nodeList = Tree.getNodeList(parent);

            let domObjects = svg.selectAll("g.node")
                .data(nodeList, (node: Node) => { return node.id; });

            let nodeEnter = Tree.enterNodes(domObjects);

            Tree.appendLabel(nodeEnter);

            expect($("#node0 text")).not.toBeNull();
            expect($("#node0 text").text()).toBe("pretty");

            expect($("#node1 text")).not.toBeNull();
            expect($("#node1 text").text()).toBe("pretty");

        });

        it('Should append the simple label text', function () {

            var svg = d3.select("body")
                .append("svg")
                .append("g");

            d3.select("body")
                .append("input")
                .attr("type", "checkbox")
                .attr("id", "labels");

            let parent = new Node(0, "simple", "pretty", null, 2, true, 2, false);
            let child = new Node(1, "simple", "pretty", parent, 0, true, 0, false);

            parent.children = [child];

            let nodeList = Tree.getNodeList(parent);

            let domObjects = svg.selectAll("g.node")
                .data(nodeList, (node: Node) => { return node.id; });

            let nodeEnter = Tree.enterNodes(domObjects);

            Tree.appendLabel(nodeEnter);

            expect($("#node0 text")).not.toBeNull();
            expect($("#node0 text").text()).toBe("simple");

            expect($("#node1 text")).not.toBeNull();
            expect($("#node1 text").text()).toBe("simple");

        });
    });

    describe('appendDecCount', function () {

        it('Should append text indicating the number of descendants of a collaped node', function () {

            var svg = d3.select("body")
                .append("svg")
                .append("g");

            let parent = new Node(0, "simple", "pretty", null, 100, true, 2, false);
            let child = new Node(1, "simple", "pretty", parent, 0, true, 0, false);

            parent._children = [child];

            let nodeList = Tree.getNodeList(parent);

            let domObjects = svg.selectAll("g.node")
                .data(nodeList, (node: Node) => { return node.id; });

            let nodeEnter = Tree.enterNodes(domObjects);

            Tree.appendDecCount(nodeEnter);

            expect($("#node0").text()).toBe("100 nodes below");
        });

        it('Should append text indicating the number of descendants of a collaped node', function () {

            var svg = d3.select("body")
                .append("svg")
                .append("g");

            let parent = new Node(0, "simple", "pretty", null, 1, true, 1, false);
            let child = new Node(1, "simple", "pretty", parent, 0, true, 0, false);

            parent.children = [child];

            let nodeList = Tree.getNodeList(parent);

            let domObjects = svg.selectAll("g.node")
                .data(nodeList, (node: Node) => { return node.id; });

            let nodeEnter = Tree.enterNodes(domObjects);

            Tree.appendDecCount(nodeEnter);

            expect($("#node0").text()).toBe("");
        });
    });

    describe('updateLocation', function () {

        it('Should update the location of the nodes', function () {

            var svg = d3.select("body")
                .append("svg")
                .append("g");

            let parent = new Node(0, "simple", "pretty", null, 100, true, 2, false);
            let child = new Node(1, "simple", "pretty", parent, 0, true, 0, false);

            parent.children = [child];


            let nodeList = Tree.getNodeList(parent);

            // console.log(nodeList);

            let domObjects = svg.selectAll("g.node")
                .data(nodeList, (node: Node) => { return node.id; });

            Tree.enterNodes(domObjects);

            Tree.updateLocation(domObjects);

            expect($("#node1").attr("transform")).toBe("translate(0,0)");
        });
    });
    describe('update circle radius', function () {

        it('Should update the radius of the circle', function () {

            var svg = d3.select("body")
                .append("svg")
                .append("g");

            let parent = new Node(0, "simple", "pretty", null, 100, true, 2, false);
            let child = new Node(1, "simple", "pretty", parent, 0, true, 0, false);
            child.decCount = 1000;

            parent.children = [child];


            let nodeList = Tree.getNodeList(parent);

            // console.log(nodeList);

            let domObjects = svg.selectAll("g.node")
                .data(nodeList, (node: Node) => { return node.id; });

            Tree.enterNodes(domObjects);
            Tree.appendCircle(domObjects);


            Tree.updateCircle(domObjects);

            expect($("#node1 circle").attr("r")).toBeGreaterThan(10);
        });
    });

    describe('update text', function () {

        it('Should update the text of the node', function () {

            var svg = d3.select("body")
                .append("svg")
                .append("g");

            let parent = new Node(0, "simple", "pretty", null, 100, true, 2, false);
            let child = new Node(1, "simple", "pretty", parent, 10, true, 2, false);

            parent.children = [child];


            let nodeList = Tree.getNodeList(parent);

            let domObjects = svg.selectAll("g.node")
                .data(nodeList, (node: Node) => { return node.id; });

            let nodeEnter = Tree.enterNodes(domObjects);
            Tree.appendLabel(nodeEnter);
            Tree.appendDecCount(nodeEnter);

            child.childCount = 0;
            child.decCount = 0;

            Tree.updateText(domObjects);

            // console.log($("#node1 text"))

            expect($("#node1 text .decCount").text()).toBe("");

        });
    });

    describe('getLinkList', function () {

        it('Should get the list of links between nodes', function () {

            let parent = new Node(0, "simple", "pretty", null, 100, true, 2, false);
            let child = new Node(1, "simple", "pretty", parent, 10, true, 2, false);

            parent.children = [child];

            let nodeList = Tree.getNodeList(parent);
            let linkList = Tree.getLinkList(nodeList);

            // console.log(linkList);
            expect(linkList[0].source).toEqual(parent);
            expect(linkList[0].target).toEqual(child);

        });
    });

    describe('enterLinks', function () {

        it('Should append links to the svg', function () {

            var svg = d3.select("body")
                .append("svg")
                .append("g");

            let parent = new Node(0, "simple", "pretty", null, 100, true, 2, false);
            let child = new Node(1, "simple", "pretty", parent, 10, true, 2, false);
            parent.children = [child];

            let nodeList = Tree.getNodeList(parent);
            let linkList = Tree.getLinkList(nodeList);

            let domObjects = svg.selectAll("g.node")
                .data(nodeList, (node: Node) => { return node.id; });

            Tree.enterNodes(domObjects);

            let linkDomObjects = Tree.svg.selectAll("path.link")
                .data(linkList, (link: any) => { return link.target.id; });

            Tree.enterLinks(linkDomObjects);

            expect(linkList.length).toEqual(1);

        });
    });



});
