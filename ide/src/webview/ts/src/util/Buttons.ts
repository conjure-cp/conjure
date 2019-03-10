import Globals from './Globals';
import Tree from './Tree';
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