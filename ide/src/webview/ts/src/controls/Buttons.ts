import Globals from '../modules/Globals';
import Tree from '../modules/Tree';
import State from '../modules/State';

declare var d3: any;

export default class Buttons {


    public static handleLabels() {
        let domObjects = $("g.node");

        for (var i = 0; i < domObjects.length; i++) {
            let id = domObjects[i].id;

            if ($("#labels").prop("checked") === true) {
                $("#" + id + " text").text(State.id2Node[Number(id.replace("node", ""))].prettyLabel);
            }
            else {
                $("#" + id + " text").text(State.id2Node[Number(id.replace("node", ""))].label);
            }
        }
        Tree.update(State.id2Node[State.rootId]);
    }

    public static handlePrettyCheckBox() {
        $("#pane").empty();
        State.pretty = !State.pretty;

        if (State.pretty) {
            Globals.lv.createUL();
            Globals.lv.update(Globals.lv.id2Node["Items"]);
        }
        else {

            if (!State.simpleDomainsAtRoot) {
                Globals.loadDomains(Globals.vscode);
            }
            else {
                Globals.lv.tabulate();
                Globals.lv.appendRows(State.simpleDomainsAtRoot);
            }
        }

        Tree.selectNode(State.selectedId);
    }


    public static bindButtons() {

    d3.select("#freeze")
        .on("change", () => {
            Globals.loadDomains(Globals.vscode);
            State.frozen = !State.frozen;
        });

    d3.select("#expressions")
        .on("change", () => {
            // Globals.loadDomains();
            // State.frozen = !State.frozen;
        });

    d3.select("#labels").on("change", this.handleLabels);

    d3.select("#check")
        .on("change", Buttons.handlePrettyCheckBox);
        //     $("#pane").empty();
        //     State.pretty = !State.pretty;

        //     if (State.pretty) {
        //         Globals.lv.createUL();
        //         Globals.lv.update(Globals.lv.id2Node["Items"]);
        //     }
        //     else {

        //         if (!State.simpleDomainsAtRoot) {
        //             Globals.loadDomains(Globals.vscode);
        //         }
        //         else {
        //             Globals.lv.tabulate();
        //             Globals.lv.appendRows(State.simpleDomainsAtRoot);
        //         }
        //     }

        //     Tree.selectNode(State.selectedId);
        }
}