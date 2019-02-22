import Node from './Node';

declare var acquireVsCodeApi: any;
declare var d3: any;

export default class Globals {
    public static vscode = acquireVsCodeApi();
    public static totalLoaded = 0;
    public static rootId = 0;
    public static selectedId = Globals.rootId;
    public static currentDomainId = 0;
    public static id2Node: {[id: number] : Node;} = {};
    // var persons: { [id: string] : IPerson; } = {};
    public static id2ChildIds: any = {};
    public static correctPath: any[] = [];
    public static viewerWidth = $(document).width();
    public static viewerHeight = $(document).height();
    public static margin = { top: 40, right: 30, bottom: 50, left: 30 };
    public static width = Globals.viewerWidth! - Globals.margin.left - Globals.margin.right;
    public static height = Globals.viewerHeight! - Globals.margin.top - Globals.margin.bottom;
    public static pathList: any[] = [];
    public static simpleDomainsAtRoot: any;
    public static init = true;
    public static pretty = true;
    public static frozen = false;
    public static i = 0;
    public static duration = 750;
    private static z : any;
    public static tree = d3.layout.tree()
        .size([Globals.height, Globals.width])
    // .nodeSize([300, 100]);
    public static waiting = false;

    public static columns = ["name", "rng"];

    public static setup = (zoom: any) => {
        Globals.z = zoom;
    }

    public static diagonal = d3.svg.diagonal()
        .projection((d: any) => {
            return [d.x, d.y];
        });

    public static expandNode = (nodeId: number) => {

        function recurse(node: any) {

            for (var i in node._children) {
                recurse(node._children[i]);
            }

            Globals.showChildren(node.id);
        }

        let node = Globals.id2Node[nodeId];
        recurse(node);
    }

    public static collapseNode = (nodeId: number) => {


        function recurse(node: any) {

            for (var i in node.children) {
                recurse(node.children[i])
            }

            Globals.hideChildren(node.id);
        }

        let node = Globals.id2Node[nodeId];
        recurse(node);

    }

    public static showChildren(nodeId: any) {

        if (Globals.id2Node[nodeId]) {
            if (Globals.id2Node[nodeId]._children) {
                Globals.id2Node[nodeId].children = Globals.id2Node[nodeId]._children;
                Globals.id2Node[nodeId]._children = null;
            }
        }
    }

    public static hideChildren(nodeId: any) {
        // console.log(nodeId);
        // console.log(exports.id2Node);
        // console.log(exports.id2Node[nodeId]);
        if (Globals.id2Node[nodeId]) {
            if (Globals.id2Node[nodeId].children) {
                Globals.id2Node[nodeId]._children = Globals.id2Node[nodeId].children;
                Globals.id2Node[nodeId].children = null;
            }
        }
    }

    public static toggleNode = (nodeId: any) => {

        if (Globals.id2Node[nodeId]._children) {
            Globals.showChildren(nodeId);
        }
        else if (Globals.id2Node[nodeId].children) {
            Globals.hideChildren(nodeId);
        }
    }

    public static focusNode = (node: any) => {
        // scale = 7;''
        // console.log(node);
        let scale = Globals.z.scale();
        let x = -node.x * scale;
        let y = -node.y * scale;

        x += Globals.width / 3;
        y += Globals.height / 2;

        d3.select('g').transition()
            .duration(Globals.duration)
            .attr("transform", "translate(" + x + "," + y + ")scale(" + scale + ")");
        Globals.z.translate([x, y]);
    }


    public static collapseFailed = () => {
        Globals.correctPath.forEach((nodeId: number) => {
            if (Globals.id2ChildIds[nodeId]) {
                Globals.id2ChildIds[nodeId].forEach((childId: number) => {
                    if (!Globals.correctPath.includes(childId)) {
                        Globals.collapseNode(childId);
                    }
                });
            }

        });

        if (!Globals.correctPath.includes(Globals.selectedId)) {

            for (var i = 0; i < Globals.correctPath.length; i++) {

                let nodeId = Globals.correctPath[i];

                if (nodeId > Globals.selectedId) {

                    Globals.selectedId = nodeId;

                    break;
                }
            };
        }
    }

    public static rightNode = () => {
        if (Globals.id2Node[Globals.selectedId].children) {
            let childCount = Globals.id2Node[Globals.selectedId].children.length;
            if (childCount > 1) {
                Globals.selectedId = Globals.id2Node[Globals.selectedId].children[childCount - 1].id;
            }
            Globals.selectNode(Globals.selectedId);
        }
    }

    public static nextNode = () => {

        let stepSize = Number($("#stepSize").val());

        if (Globals.id2Node[Globals.selectedId]._children) {
            // console.log("NOW")
            Globals.toggleNode(Globals.selectedId);
            return
        }

        if (!Globals.id2Node[Globals.selectedId + stepSize]) {
            Globals.loadNNodes();
            // console.log("NEXT")
        }
        else {
            // exports.selectedId = exports.currentId;
            console.log("here!!!")
            Globals.selectedId += stepSize;
            Globals.selectNode(Globals.selectedId);
        }
    }

    //TODO check that the previous node is not withn a collapsed node
    public static previousNode = () => {
        if (Globals.selectedId > 1) {
            Globals.selectedId--;
            Globals.selectNode(Globals.selectedId);
        }
    }

    public static upNode = () => {
        if (Globals.selectedId > Globals.rootId) {
            // exports.selectedId = exports.id2Parent[exports.selectedId].id;
            Globals.selectedId = Globals.id2Node[Globals.selectedId].parent.id;
        }
        Globals.selectNode(Globals.selectedId);
    }

    public static loadNNodes = () => {

        if (!Globals.waiting) {

            // console.log("requesting more");

            Globals.vscode.postMessage({
                command: 'loadNodes',
                amount: Number($("#stepSize").val()),
                start: Globals.selectedId
                // start: exports.currentId
            });

            // console.log("SET WAIT TRUE NODES");
            Globals.waiting = true;
        }
        else {
            // console.log("waiting");

        }
    }

    public static selectNode = (nodeId: number) => {
        Globals.selectedId = nodeId;

        let allCircles = ".node circle"
        d3.selectAll(allCircles).classed("selected", false);
        let s = "#node" + nodeId + " circle";
        d3.select(s).classed("selected", true);

        // console.log(nodeId)
        // console.log(exports.id2Node[nodeId])

        Globals.focusNode(Globals.id2Node[nodeId]);


        Globals.currentDomainId = 0;

        // console.log("Calling load domains");
        // if (!exports.pretty) {
        //     $("#pane").empty();
        //     exports.tabulate()
        // }

        if (!Globals.frozen) {
            Globals.loadDomains();
        }

    }

    public static loadDomains = () => {

        // console.log(exports.waiting);

        if (!Globals.waiting) {

            if (Globals.pretty) {
                Globals.sendPrettyRequest()
            }
            else {
                Globals.sendSimpleRequest()
            }

            Globals.waiting = true;
        }
        // console.log("SET WAIT TRUE DOMAINS, pretty: " + exports.pretty );
    }
    public static sendSimpleRequest = () => {
        Globals.vscode.postMessage({
            command: "simpleDomains",
            amount: Number($("#domCount").val()),
            start: Globals.currentDomainId,
            nodeId: Globals.selectedId,
        });
    }

    public static sendPrettyRequest = () => {
        Globals.vscode.postMessage({
            command: "prettyDomains",
            nodeId: Globals.selectedId,
            paths: Globals.pathList.join(":")
        });
    }



    public static tabulate = () => {
        var table = d3.select('#pane').append('table')
        var thead = table.append('thead')

        // append the header row
        thead.append('tr')
            .selectAll('th')
            .data(Globals.columns).enter()
            .append('th')
            .text(function (column: any) { return column; });
    }

    public static appendRows = (data: any) => {
        var table = d3.select('#pane').append('table');
        var tbody = table.append('tbody');

        var rows = tbody.selectAll('tr')
            .data(data)
            .enter()
            .append('tr')
            .attr("id", (d: any, i: any) => {
                // console.log(d);
                // return "row" + (i + exports.currentDomainId - Number($("#domCount").val())) 
                return d.name;
            })

        // create a cell in each row for each column
        var cells = rows.selectAll('td')
            .data((row: any) => {
                return Globals.columns.map((column) => {
                    //dasdas
                    ////dasdas
                    return { column: column, value: row[column] };
                });
            })
            .enter()
            .append('td')
            .text((d: any) => { return d.value; });
    }

    public static addNode = (nodeId: number, parentId: number, label: string) => {

        Globals.totalLoaded++;
        // let newNode = { id: nodeId, name: label, parent: Globals.id2Node[parentId] };
        let newNode = new Node(nodeId, label, Globals.id2Node[parentId]);
        // console.log(exports.currentId);
        // console.log(parentId);
        // console.log(exports.id2Node);

        if (parentId === -1) {
            Globals.id2Node[nodeId] = newNode;
            return;
        }

        if (!Globals.id2Node[parentId].children) {
            Globals.id2Node[parentId].children = [];
        }

        Globals.id2Node[parentId].children.push(newNode);
        Globals.id2Node[nodeId] = newNode;
        // exports.id2Parent[nodeId] = exports.id2Node[parentId];
    }

    public static getChildren = (parentId: number) => {
        Globals.vscode.postMessage({
            command: 'children',
            parentId: parentId,
        });
    }

    public static initialize = () => {
        Globals.vscode.postMessage({
            command: 'init',
        });


        Globals.vscode.postMessage({
            command: 'correctPath',
        });

        Globals.vscode.postMessage({
            command: 'longestBranchingVariable',
        });

        Globals.vscode.postMessage({
            command: 'loadCore',
        });

        Globals.vscode.postMessage({
            command: 'simpleDomains',
            amount: Number($("#domCount").val()),
            start: 0,
            nodeId: Globals.rootId,
        });

    }

}

Globals.initialize();



// exports.id2Parent = {};











