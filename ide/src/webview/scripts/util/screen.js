import globals from "./globals"
import * as listView from "./listView"


export function appendControls() {


    console.log("CONTROLS");
    var step = d3.select("#controls")
        .append('div')
        .classed('col-xs-1', true);

    // console.log(step);
    step.append('input')
        .classed('form-control', true)
        .attr('type', 'text')
        .attr('name', 'textInput')
        .attr('value', '1')
        .attr('id', 'stepSize')

    step.append('input')
        .classed('form-control', true)
        .attr('type', 'text')
        .attr('name', 'textInput')
        .attr('value', '100')
        .attr('id', 'domCount')

    d3.select("#controls")
        .append('label')
        .text("Pretty")
        .append("input")
        .attr("checked", false)
        .attr("type", "checkbox")
        .attr("id", "check")
        .on("change", () => {
            $("#pane").empty();
            globals.pretty = !globals.pretty;

            let temp = globals.selectedId;

            // globals.selectNode(1);

            if (globals.pretty){
                listView.createUL()
                let r = listView.getRootNode();
                console.log(r);
                listView.render(r, r);
            }

            globals.selectNode(temp);
            // console.log("Changed!")
            // showDomains(selectedNode)
        })
        // .attr("onClick", () => {
        //     console.log("hello");
        //     showDomains(selectedNode)
        // });
}
        // d3.select("#controls")
        //     .append("input")
        //     .attr("type", "button")
        //     .attr("value", "Collapse All")
        //     .on("click", collapser);

//         d3.select("#controls")
//             .append("input")
//             .attr("type", "button")
//             .attr("value", "Expand All")
//             .on("click", expander);

//         d3.select("#controls")
//             .append("input")
//             .attr("type", "button")
//             .attr("value", "Find Root")
//             .on("click", () => {
//                 selectNode(root.minionID);
//                 focusNode(id2Node[root.minionID]);
//             });

//         d3.select("#controls")
//             .append("input")
//             .attr("type", "button")
//             .attr("value", "Previous")
//             .on("click", () => {
//                 previous();
//             });

//         d3.select("#controls")
//             .append("input")
//             .attr("type", "button")
//             .attr("value", "Next")
//             .on("click", () => {
//                 next();
//             });

//         d3.select("#controls")
//             .append("input")
//             .attr("type", "button")
//             .attr("value", "Toggle")
//             .on("click", () => {
//                 nodeToggle(id2Node[selectedNode]);
//             });

//         d3.select("#controls")
//             .append('label')
//             .text("Pretty")
//             .append("input")
//             .attr("checked", true)
//             .attr("type", "checkbox")
//             .attr("id", "check")
//             .on("change", () => {
//                 // console.log("Changed!")
//                 showDomains(selectedNode)
//             })
//         // .attr("onClick", () => {
//         //     console.log("hello");
//         //     showDomains(selectedNode)
//         // });