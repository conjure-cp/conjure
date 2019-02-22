import Node from './Node';
import Data from './Data';
import Tree from './Tree';

declare var acquireVsCodeApi: any;
declare var d3: any;

export default class Globals {
    public static data = new Data();
    public static vscode = acquireVsCodeApi();

    public static nextNode() {

        let stepSize = Number($("#stepSize").val());

        if (Globals.data.id2Node[Globals.data.selectedId]._children) {
            Globals.data.toggleNode(Globals.data.selectedId);
            return;
        }

        if (!Globals.data.id2Node[Globals.data.selectedId + stepSize]) {
            Globals.loadNNodes();
        }
        else {
            Globals.data.selectedId += stepSize;
            Tree.selectNode(Globals.data.selectedId);
        }
    }

    public static previousNode() {

        let prevId = Globals.data.selectedId - 1;

        if (Globals.data.id2Node[prevId]){
            Globals.data.selectedId--;
            Tree.selectNode(Globals.data.selectedId);
        }
    }

    public static rightNode() {
        if (Globals.data.id2Node[Globals.data.selectedId].children) {
            let childCount = Globals.data.id2Node[Globals.data.selectedId].children.length;
            if (childCount > 1) {
                Globals.data.selectedId = Globals.data.id2Node[Globals.data.selectedId].children[childCount - 1].id;
            }
            Tree.selectNode(Globals.data.selectedId);
        }
    }

    public static upNode() {
        if (Globals.data.selectedId > Globals.data.rootId) {
            Globals.data.selectedId = Globals.data.id2Node[Globals.data.selectedId].parent.id;
        }
        Tree.selectNode(Globals.data.selectedId);
    }

    public static loadNNodes() {

        if (!Globals.data.waiting) {

            Globals.vscode.postMessage({
                command: 'loadNodes',
                amount: Number($("#stepSize").val()),
                start: Globals.data.selectedId
            });

            Globals.data.waiting = true;
        }
    }

    public static loadDomains() {

        if (!Globals.data.waiting) {

            if (Globals.data.pretty) {
                Globals.sendPrettyRequest();
            }
            else {
                Globals.sendSimpleRequest();
            }

            Globals.data.waiting = true;
        }
    }

    public static sendSimpleRequest() {
        Globals.vscode.postMessage({
            command: "simpleDomains",
            amount: Number($("#domCount").val()),
            start: Globals.data.currentDomainId,
            nodeId: Globals.data.selectedId,
        });
    }

    public static sendPrettyRequest() {
        Globals.vscode.postMessage({
            command: "prettyDomains",
            nodeId: Globals.data.selectedId,
            paths: Globals.data.pathList.join(":")
        });
    }

    public static tabulate() {
        var table = d3.select('#pane').append('table')
        var thead = table.append('thead')

        // append the header row
        thead.append('tr')
            .selectAll('th')
            .data(Globals.data.columns).enter()
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
            .attr("id", (d: any, i: any) => { return d.name; })

        // create a cell in each row for each column
        var cells = rows.selectAll('td')
            .data((row: any) => {
                return Globals.data.columns.map((column) => {
                    return { column: column, value: row[column] };
                });
            })
            .enter()
            .append('td')
            .text((d: any) => { return d.value; });
    }

    public static requestChildren(parentId: number) {
        Globals.vscode.postMessage({
            command: 'children',
            parentId: parentId,
        });
    }

    public static initialize() {
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
            nodeId: Globals.data.rootId,
        });
    }
}