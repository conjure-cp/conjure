import Globals from './Globals';
import Tree from './Tree';
import Node from './Node';

declare var d3: any;

export default class Buttons {

    public static bindButtons() {

        d3.select("#freeze")
            .on("change", () => {
                Globals.loadDomains();
                Globals.s.frozen = !Globals.s.frozen;
            });

        d3.select("#expressions")
            .on("change", () => {
                // Globals.loadDomains();
                // Globals.s.frozen = !Globals.s.frozen;
            });

        d3.select("#labels")
            .on("change", () => {
                let domObjects = $("g.node");

                for (var i = 0; i < domObjects.length; i++) {
                    let id = domObjects[i].id;

                    if ($("#labels").prop("checked") === true) {
                        $("#" + id + " text").text(Globals.s.id2Node[Number(id.replace("node", ""))].prettyLabel);
                    }
                    else {
                        $("#" + id + " text").text(Globals.s.id2Node[Number(id.replace("node", ""))].label);
                    }

                    console.log("YOOO")

                }

                Tree.update(Globals.s.id2Node[Globals.s.rootId]);
            });

        d3.select("#check")
            .on("change", () => {
                $("#pane").empty();
                Globals.s.pretty = !Globals.s.pretty;

                if (Globals.s.pretty) {
                    Globals.lv.createUL();
                    // let r = Globals.lv.getRootNode();
                    // console.log(r);
                    Globals.lv.update(Globals.lv.id2Node["Items"]);
                }
                else {

                    if (!Globals.s.simpleDomainsAtRoot) {
                        Globals.loadDomains();
                    }
                    else {
                        Globals.tabulate();
                        Globals.appendRows(Globals.s.simpleDomainsAtRoot);
                    }
                }

                Tree.selectNode(Globals.s.selectedId);
            });
    }
}
