import "./treelist"
// import globals from "./globals"
declare var jsPanel: any;
declare var d3: any;

import Globals from './Globals';

export default class Listview {

    public static setNodeId() {
        panel.setHeaderTitle("Node: " + Globals.selectedId);
    }

    public static render(data: any, parent: any) {
        // console.log("CALLED RENDER");
        // console.log(JSON.stringify(data));

        if (init) {
            init = false;
            rootNode = data;
        }

        var nodes = tree.nodes(data),
            duration = 250;

        function toggleChildren(d: any) {
            // console.log("start")
            // console.log(d)
            if (d.children) {
                d._children = d.children;
                d.children = null;
            } else if (d._children) {
                d.children = d._children;
                d._children = null;
            }
            // console.log("end")
            // console.log(d)
        }

        var nodeEls = ul.selectAll("li.node").data(nodes, function (d: any) {
            d.id = d.id || id++;
            return d.id;
        });
        //entered nodes
        var entered = nodeEls.enter().append("li").classed("node", true)
            .attr("id", (d: any) => {
                var name = d.name;
                if (d.parent) {
                    d.parent.children.forEach((element: any) => {
                        if (element.name == "Cardinality") {
                            // console.log(d.name)
                            // console.log(element)
                            name = d.parent.name + d.name;
                        }
                    })
                }

                d["domIdentifier"] = name;

                return name;
            })
            // .style("margin-bottom", "200px")
            .style("top", parent.y + "px")
            .style("opacity", 0)
            .style("height", tree.nodeHeight() + "px")
            .on("click", function (d: any) {

                // check if its a baby set
                // console.log(d._children)
                if (d._children) {
                    if (d._children.length == 0) {

                        let p = getVarPath(d);

                        Globals.pathList.push(p);

                        Globals.vscode.postMessage({
                            command: 'loadSet',
                            nodeId: Globals.selectedId,
                            path: p
                        });
                    }
                }

                toggleChildren(d);
                Listview.render(data, d);
                if (Globals.selectedId != Globals.rootId) {
                    Listview.setChanged();
                }

            })
            .on("mouseover", function (d: any) {
                // d3.select(this).classed("changed", true);
            })
            .on("mouseout", function (d: any) {
                // d3.selectAll(".changed").classed("changed", false);
            })
            .each((d: any) => {
                path2Node[d.domIdentifier] = d;
            })
        //add arrows if it is a folder
        entered.append("span").attr("class", function (d: any) {
            var icon = d.children ? "fas fa-chevron-down"
                : d._children ? "fas fa-chevron-right" : "";
            return "caret " + icon;
        });

        entered.append("span").attr("class", "filename")
            .html(function (d: any) { return d.name; });

        //  update the ranges

        d3.selectAll("span.filename").html((d: any) => {
            // console.log("D is  " + d.name);
            return d.name;
        })

        //update caret direction
        nodeEls.select("span.caret").attr("class", function (d: any) {
            var icon = d.children ? "fas fa-chevron-down"
                : d._children ? "fas fa-chevron-right" : "";
            return "caret " + icon;
        });
        //update position with transition
        nodeEls.transition().duration(duration)
            .style("top", function (d: any) { return (d.y - tree.nodeHeight()) + "px"; })
            .style("left", function (d: any) { return d.x + "px"; })
            .style("opacity", 1);
        nodeEls.exit().remove();

    }

    public static createUL() {
        init = true;
        ul = d3.select("#pane").append("ul").classed("treelist", "true");
    }

    public static getRootNode() {
        return rootNode;
    }

    public static setChangedExpressions(expressions: any) {
        // console.log(expressions)
        // console.log(name2Node["Changed Expressions"])
        path2Node["Changed Expressions"]["children"] = expressions

    }

    public static updateNodes(data: any) {

        data.forEach((element: any) => {

            if (element.hasOwnProperty("Cardinality")) {
                // if (!name2Node[element.name].children){
                // console.log(name2Node[element.name]);
                // }
                if (path2Node[element.name].children) {

                    let setNode = path2Node[element.name].children
                    // console.log(element)

                    setNode[1].children[0].name = element.Cardinality

                    // if (setNode.children.length > 3){
                    if (element.Included) {
                        setNode[2].children[0].name = element.Included
                        setNode[3].children[0].name = element.Excluded
                    }

                    if (element.Children && !setNode[2].children) {
                        setNode[2] = { name: "Children", children: element.Children.children }
                    }
                }
            }
            else {
                path2Node[element.name].children[0].name = element.rng;
            }

        });

        Listview.render(rootNode, rootNode);
    }

    public static setChangedList(list: any) {
        changedList = list
    }

    public static setChanged() {

        // console.log(changedList)

        d3.selectAll("li").classed("changed", false);

        var ancestors: any = []


        changedList.forEach((name: any) => {

            ancestors.push(name);
            // console.log(name)

            var obj = path2Node[name].parent;

            while (obj) {

                // console.log(ancestors)
                if (!ancestors.includes(obj.domIdentifier)) {
                    ancestors.push(obj.domIdentifier);
                }
                obj = obj.parent;
            }

            ancestors.forEach((element: any) => {
                d3.select('[id="' + element + '"]').classed("changed", true);
            })

        });
    }

}


let panel = jsPanel.create({
    theme: "black " + ' filled',
    headerTitle: 'my panel #1',
    position: 'right-top 0 58',
    contentSize: {
        // width: 450,
        width: () => {
            return $(document).width()! / 3;
        },
        height: () => {
            return $(document).height()! * 0.9;
        }
    },
    content: `
    <div id="pane"> </div>`
});


var id = 0;
var tree = d3.layout.treelist()
    .childIndent(15)
    .nodeHeight(40);

var init = true;
var rootNode: any;

var path2Node: any = {}

var changedList: any = [];

var ul: any;

window.addEventListener('message', event => {
    const message = event.data

    switch (message.command) {
        case 'loadSet':
            path2Node[message.data.structure.name].children = message.data.structure.children;
            Listview.render(rootNode, rootNode);
            Listview.updateNodes([message.data.update])
            Globals.sendPrettyRequest();
            break;
    }

});


function getVarPath(node: any) {

    let path: any = []

    function recurse(node: any) {

        if (node.name != "Children" && node.name != "Items") {
            path.push(node.name);
        }

        if (node.parent) {
            if (node.parent.name === "Domain Variables") {
                return node;
            }

            recurse(node.parent);
        }
    }

    recurse(node);
    return path.reverse().join(".");
}

Listview.createUL();