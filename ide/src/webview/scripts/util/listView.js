import colours from './colours.js';
import "./treelist"
import globals from "./globals"


let panel = jsPanel.create({
    theme: colours.bgColour + ' filled',
    headerTitle: 'my panel #1',
    position: 'right-top 0 58',
    contentSize: {
        // width: 450,
        width: () => {
            return $(document).width() / 3;
        },
        height: () => {
            return $(document).height() * 0.9;
        }
    },
    content: `
    <div id="pane"> </div>`
});

export function setNodeId() {
    panel.setHeaderTitle("Node: " + globals.selectedId);
}

var id = 0;
var tree = d3.layout.treelist()
    .childIndent(15)
    .nodeHeight(40);

var init = true;
var rootNode;

var path2Node = {}

var changedList;

var ul;

window.addEventListener('message', event => {
    const message = event.data

    switch (message.command) {
        case 'loadSet':
            // console.log("structure")
            // console.log(message.data.structure)

            path2Node[message.data.structure.name].children = message.data.structure.children;

            // console.log("updated: ")
            // console.log(path2Node[message.data.structure.name]);

            render(rootNode, rootNode);

            updateNodes([message.data.update])

            globals.sendPrettyRequest();

            break;
    }

});

export function createUL() {
    init = true;
    ul = d3.select("#pane").append("ul").classed("treelist", "true");
}

export function getRootNode() {
    return rootNode;
}

export function setChangedExpressions(expressions) {
    // console.log(expressions)
    // console.log(name2Node["Changed Expressions"])
    path2Node["Changed Expressions"]["children"] = expressions

}

export function updateNodes(data) {

    // console.log(data)

    data.forEach(element => {

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

    render(rootNode, rootNode);
}

export function setChangedList(list) {
    changedList = list
}

export function setChanged() {

    // console.log(changedList)

    d3.selectAll("li").classed("changed", false);

    var ancestors = []


    changedList.forEach(name => {

        ancestors.push(name);
        // console.log(name)

        var obj = path2Node[name].parent;

        while (obj) {

            console.log(ancestors)
            if (!ancestors.includes(obj.label)) {
                ancestors.push(obj.label);
            }
            obj = obj.parent;
        }

        ancestors.forEach(element => {
            d3.select('[id="' + element + '"]').classed("changed", true);
        })

        // console.log(d3.select('[id="' + element + '"]' ).classed("changed"));

    });
}

export function render(data, parent) {
    console.log("CALLED RENDER");
    // console.log(JSON.stringify(data));

    if (init) {
        init = false;
        rootNode = data;
    }

    var nodes = tree.nodes(data),
        duration = 250;

    function toggleChildren(d) {
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

    var nodeEls = ul.selectAll("li.node").data(nodes, function (d) {
        d.id = d.id || id++;
        return d.id;
    });
    //entered nodes
    var entered = nodeEls.enter().append("li").classed("node", true)
        .attr("id", (d) => {
            var name = d.name;
            if (d.parent) {
                d.parent.children.forEach(element => {
                    if (element.name == "Cardinality") {
                        // console.log(d.name)
                        // console.log(element)
                        name = d.parent.name + d.name;
                    }
                })
            }

            d["label"] = name;

            return name;
        })
        // .style("margin-bottom", "200px")
        .style("top", parent.y + "px")
        .style("opacity", 0)
        .style("height", tree.nodeHeight() + "px")
        .on("click", function (d) {

            // check if its a baby set
            // console.log(d._children)
            if (d._children) {
                if (d._children.length == 0) {

                    let p = getVarPath(d);

                    // console.log(p)

                    globals.pathList.push(p);

                    globals.vscode.postMessage({
                        command: 'loadSet',
                        nodeId: globals.selectedId,
                        path: p
                        // path: d.name,
                    });
                }
            }

            toggleChildren(d);
            render(data, d);
            if (globals.selectedId != globals.rootId) {
                setChanged();
            }

        })
        .on("mouseover", function (d) {
            // d3.select(this).classed("changed", true);
        })
        .on("mouseout", function (d) {
            // d3.selectAll(".changed").classed("changed", false);
        })
        .each((d) => {
            // console.log(d)
            // if (d.Cardinality) {
            // path2Node[getVarPath(d)] = d;
            path2Node[d.label] = d;
            // }
            // else{
            // path2Node[d.name] = d;
            // }
        })
    //add arrows if it is a folder
    entered.append("span").attr("class", function (d) {
        var icon = d.children ? "fas fa-chevron-down"
            : d._children ? "fas fa-chevron-right" : "";
        return "caret " + icon;
    });
    //add icons for folder for file
    // entered.append("span").attr("class", function (d) {
    //     var icon = d.children || d._children ? "glyphicon-folder-close"
    //         : "glyphicon-file";
    //     return  icon;
    // });
    //add text
    entered.append("span").attr("class", "filename")
        .html(function (d) { return d.name; });


    //  update the ranges

    d3.selectAll("span.filename").html((d) => {
        // console.log("D is  " + d.name);

        return d.name;


    })


    //update caret direction
    nodeEls.select("span.caret").attr("class", function (d) {
        var icon = d.children ? "fas fa-chevron-down"
            : d._children ? "fas fa-chevron-right" : "";
        return "caret " + icon;
    });
    //update position with transition
    nodeEls.transition().duration(duration)
        .style("top", function (d) { return (d.y - tree.nodeHeight()) + "px"; })
        .style("left", function (d) { return d.x + "px"; })
        .style("opacity", 1);
    nodeEls.exit().remove();

}

function getVarPath(node) {

    let path = []

    function recurse(node) {

        // console.log(node.name);

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

    recurse(node)
    return path.reverse().join(".");
}

createUL();

// let data = {name: "doggo", _children: [{name: "dasdasda", _children : []}]};
// render(data, data);
// console.log("RENDERING")
