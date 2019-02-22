import Globals from './Globals';
import Tree from './Tree';
import Listview from './Listview';

declare var d3: any;

export default class Buttons {

    public static bindButtons() {

        d3.select("#freeze")
            .on("change", () => {
                Globals.loadDomains();
                Globals.data.frozen = !Globals.data.frozen;
            })
        d3.select("#check")
            .on("change", () => {
                $("#pane").empty();
                Globals.data.pretty = !Globals.data.pretty;

                if (Globals.data.pretty) {
                    Listview.createUL();
                    let r = Listview.getRootNode();
                    console.log(r);
                    Listview.render(r, r);
                }
                else {

                    if (!Globals.data.simpleDomainsAtRoot) {
                        Globals.loadDomains();
                    }
                    else {
                        Globals.tabulate();
                        Globals.appendRows(Globals.data.simpleDomainsAtRoot);
                    }
                }

                Tree.selectNode(Globals.data.selectedId);
            });
    }
}
