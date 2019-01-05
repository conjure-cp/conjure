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

            globals.selectNode(globals.selectedId);
        })
}