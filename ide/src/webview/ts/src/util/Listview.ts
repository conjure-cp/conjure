import "./treelist";
import Globals from './Globals';
import Node from './Node';

declare var jsPanel: any;
declare var d3: any;

var id = 0;
var tree = d3.layout.treelist()
    .childIndent(15)
    .nodeHeight(40);

var init = true;
var rootNode: any;

var id2Node: any = {}

var changedList: any = [];

var ul: any;

window.addEventListener('message', event => {
    const message = event.data

    switch (message.command) {
        case 'loadSet':
            id2Node[message.data.structure.name].children = message.data.structure.children;
            Listview.update(rootNode);
            Listview.updateNodes([message.data.update])
            Globals.sendPrettyRequest();
            break;
    }

});


export default class Listview {

    public static duration = 250;

    public static panel = jsPanel.create({
        theme: getComputedStyle(document.body).getPropertyValue('--background-color') + ' filled',
        headerTitle: 'my panel #1',
        position: 'right-top 0 58',
        contentSize: {
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

    public static updatePanelTitle() {
        Listview.panel.setHeaderTitle("Node: " + Globals.s.selectedId);
    }

    public static getSetPath(node: Node) {

        let path: string[] = [];

        function recurse(node: Node) {

            if (node.name !== "Children" && node.name !== "Items") {
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


    public static update(source: Node) {

        if (init) {
            init = false;
            rootNode = source;
        }

        var nodes = tree.nodes(source);

        var nodeEls = ul.selectAll("li.node")
            .data(nodes, function (node: Node) {
                node.id = node.id || id++;
                return node.id;
            });


        var entered = nodeEls.enter().append("li").classed("node", true)
            .attr("id", (node: Node) => {
                let name = node.name;
                if (node.parent) {
                    node.parent.children.forEach((element: Node) => {
                        if (element.name === "Cardinality") {
                            name = node.parent.name + node.name;
                        }
                    });
                }
                // node["domIdentifier"] = name;
                return name;
            })
            // .style("margin-bottom", "200px")
            .style("top", () => {
                let suffix = "px";
                if (source.parent) {
                    return source.parent.y + suffix;
                }
                return source.y + suffix;
            })
            .style("opacity", 0)
            .style("height", tree.nodeHeight() + "px")
            .on("click", function (d: any) {

                // check if its a baby set
                // console.log(d._children)
                if (d._children) {
                    if (d._children.length === 0) {

                        let p = Listview.getSetPath(d);

                        Globals.s.pathList.push(p);

                        Globals.vscode.postMessage({
                            command: 'loadSet',
                            nodeId: Globals.s.selectedId,
                            path: p
                        });
                    }
                }

                Node.toggleNode(d);
                Listview.update(source);
                if (Globals.s.selectedId !== Globals.s.rootId) {
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
                id2Node[d.name] = d;
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
        nodeEls.transition().duration(Listview.duration)
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
        id2Node["Changed Expressions"]["children"] = expressions

    }

    public static updateNodes(data: any) {

        data.forEach((element: any) => {

            if (element.hasOwnProperty("Cardinality")) {
                // if (!name2Node[element.name].children){
                // console.log(name2Node[element.name]);
                // }
                if (id2Node[element.name].children) {

                    let setNode = id2Node[element.name].children
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
                id2Node[element.name].children[0].name = element.rng;
            }

        });

        Listview.update(rootNode);
    }

    public static setChangedList(list: any) {
        changedList = list;
    }

    public static setChanged() {

        // console.log(changedList)

        d3.selectAll("li").classed("changed", false);

        var ancestors: any = [];

        changedList.forEach((name: any) => {

            ancestors.push(name);

            if (id2Node[name]) {

                var obj = id2Node[name].parent;

                while (obj) {

                    // console.log(ancestors)
                    if (!ancestors.includes(obj.name)) {
                        ancestors.push(obj.name);
                    }
                    obj = obj.parent;
                }
            }

            ancestors.forEach((element: any) => {
                d3.select('[id="' + element + '"]').classed("changed", true);
            })

        });
    }

}


Listview.createUL();