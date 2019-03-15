import Node from '../testable/Node';
import State from '../testable/State';
import Tree from './Tree';
import Listview from './Listview';

declare var acquireVsCodeApi: any;
declare var d3: any;

export default class Globals {
    // public static s = new State();
    public static lv = new Listview();
    public static vscode = acquireVsCodeApi();
    public static columns = ["Name", "Domain"];
    // public static columns = ["name", "rng"];

    public static tabulate() {
        var table = d3.select('#pane').append('table');
        var thead = table.append('thead');

        // append the header row
        thead.append('tr')
            .selectAll('th')
            .data(Globals.columns).enter()
            .append('th')
            .text(function (column: any) { return column; });
    }

    public static appendRows(data: any) {
        var table = d3.select('#pane').append('table');
        var tbody = table.append('tbody');

        var rows = tbody.selectAll('tr')
            .data(data)
            .enter()
            .append('tr')
            .attr("id", (d: any, i: any) => { return d.name; });

        // create a cell in each row for each column
        rows.selectAll('td')
            .data((row: any) => {
                return Globals.columns.map((column) => {
                    let val;

                    if (column === "Domain"){
                        val = row["rng"];
                    }

                    if (column === "Name"){
                        val = row["name"];
                    }

                    return { column: column, value: val };
                });
            })
            .enter()
            .append('td')
            .text((d: any) => { return d.value; });
    }

    public static previousSolutionNode(){

        if (State.solNodIds.length === 0){
            return;
        }

        if (!State.solNodIds.includes(State.selectedId)){
            State.selectedId = State.solNodIds[State.solNodIds.length - 1];
            return;
        }

        let currentSolId = State.solNodIds.indexOf(State.selectedId);

        if (currentSolId - 1 >= 0){
            State.selectedId = State.solNodIds[currentSolId - 1];
        }
    }


    public static nextSolutionNode(){

        if (State.solNodIds.length === 0){
            return;
        }

        if (!State.solNodIds.includes(State.selectedId)){
            State.selectedId = State.solNodIds[0];
            return;
        }

        let currentSolId = State.solNodIds.indexOf(State.selectedId);

        if (currentSolId + 1 < State.solNodIds.length){
            State.selectedId = State.solNodIds[currentSolId + 1];
        }
    }


    public static nextNode() {

        // let stepSize = Number($("#stepSize").val());
        let stepSize = 1;

        let node = State.id2Node[State.selectedId];

        // console.log("current");
        // console.log(node);

        if (node._children) {
            Node.toggleNode(node);
            return;
        }

        if (!State.id2Node[State.selectedId + stepSize]) {
            Globals.loadNNodes();
        }
        else {
            State.selectedId += stepSize;
            Tree.selectNode(State.selectedId);
        }
    }

    public static previousNode() {

        let prevId = State.selectedId - 1;

        if (State.id2Node[prevId]) {
            State.selectedId--;
            Tree.selectNode(State.selectedId);
        }
    }

    public static rightNode() {
        if (State.id2Node[State.selectedId].children) {
            let childCount = State.id2Node[State.selectedId].children!.length;
            if (childCount > 1) {
                State.selectedId = State.id2Node[State.selectedId].children![childCount - 1].id;
            }
            Tree.selectNode(State.selectedId);
        }
    }

    public static upNode() {
        if (State.selectedId > State.rootId) {
            State.selectedId = State.id2Node[State.selectedId].parent!.id;
        }
        Tree.selectNode(State.selectedId);
    }

    public static loadNNodes() {

        if (!State.waiting) {

            Globals.vscode.postMessage({
                command: 'loadNodes',
                // amount: Number($("#stepSize").val()),
                amount: 1,
                start: State.selectedId
            });

            State.waiting = true;
        }
    }

    public static loadDomains() {

        if (!State.waiting) {

            if (State.pretty) {
                Globals.sendPrettyRequest();
            }
            else {
                Globals.sendSimpleRequest();
            }

            State.waiting = true;
        }
    }

    public static sendSimpleRequest() {
        Globals.vscode.postMessage({
            command: "simpleDomains",
            nodeId: State.selectedId,
            wantExpressions: !$("#expressions").prop("checked"),
        });
    }

    public static sendPrettyRequest() {
        Globals.vscode.postMessage({
            command: "prettyDomains",
            nodeId: State.selectedId,
            wantExpressions: !$("#expressions").prop("checked"),
            paths: State.pathList.join(":")
        });
    }

    public static loadChildIds(nodeId: number) {
        Globals.vscode.postMessage({
            command: 'loadChildren',
            id: nodeId,
        });
    }

    public static initialize() {
        Globals.vscode.postMessage({
            command: 'init',
        });

        Globals.vscode.postMessage({
            command: 'longestBranchingVariable',
        });

        // Globals.vscode.postMessage({
        //     command: 'loadCore',
        // });
    }
}