import globals from "./globals"
import * as listView from "./listView"


export function appendControls() {
    d3.select("#check")
        .on("change", () => {
            $("#pane").empty();
            globals.pretty = !globals.pretty;

            if (globals.pretty) {
                listView.createUL()
                let r = listView.getRootNode();
                console.log(r);
                listView.render(r, r);
            }
            else {

                if (!globals.simpleDomainsAtRoot) {
                    globals.loadDomains(globals.selectedId);
                }
                else {
                    globals.tabulate()
                    globals.appendRows(globals.simpleDomainsAtRoot);
                }
                // globals.vscode.postMessage({
                //     command: 'simpleDomains',
                //     amount: Number($("#domCount").val()),
                //     start: 0,
                //     nodeId: 1,
                // });
            }

            globals.selectNode(globals.selectedId);
        })
}