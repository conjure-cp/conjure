import Globals from './Globals';
import Tree from './Tree';
import Listview from './Listview';

declare var d3: any;

export default class Buttons {

    public static bindButtons() {

        d3.select("#freeze")
            .on("change", () => {
                Globals.loadDomains();
                Globals.s.frozen = !Globals.s.frozen;
            });

        d3.select("#check")
            .on("change", () => {
                $("#pane").empty();
                Globals.s.pretty = !Globals.s.pretty;

                if (Globals.s.pretty) {
                    Listview.createUL();
                    let r = Listview.getRootNode();
                    console.log(r);
                    Listview.update(r);
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
