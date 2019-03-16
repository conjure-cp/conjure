import Globals from '../testable/Globals';
import Tree from '../testable/Tree';
import State from '../testable/State';
// import Node from './Node';
// import d3_save_svg from '../../node_modules/d3-save-svg';
let s = require( '../../node_modules/save-svg-as-png/lib/saveSvgAsPng.js');

// let s = require('./save');

// import * from './save' as Save

// let save = require()


declare var d3: any;

export default class Buttons {

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

        d3.select("#labels")
            .on("change", () => {
                let domObjects = $("g.node");

                for (var i = 0; i < domObjects.length; i++) {
                    let id = domObjects[i].id;

                    if ($("#labels").prop("checked") === true) {
                        $("#" + id + " text").text(State.id2Node[Number(id.replace("node", ""))].prettyLabel);
                    }
                    else {
                        $("#" + id + " text").text(State.id2Node[Number(id.replace("node", ""))].label);
                    }

                    console.log("YOOO")

                }

                Tree.update(State.id2Node[State.rootId]);
            });

        d3.select("#check")
            .on("change", () => {
                $("#pane").empty();
                State.pretty = !State.pretty;

                if (State.pretty) {
                    Globals.lv.createUL();
                    // let r = Globals.lv.getRootNode();
                    // console.log(r);
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
            });

            d3.select('#saveButton').on('click', function() {
                s.saveSvgAsPng(d3.select('#theTree').node(), "diagram.png");
            //     var config = {
            //       filename: 'customFileName',
            //     }
            //     d3_save_svg.save(d3.select('svg').node(), config);
                console.log("saved");
              });
    }

}