import "../util/treelist";
import Globals from './Globals';
import State from './State';
import Node from './Node';

declare var jsPanel: any;
declare var d3: any;

export default class Listview {

    public nodeCount = 0;
    public init = true;
    public id2Node: { [id: string]: any; } = {};

    // public static changedList: any = [];
    public tree: any;
    public ul: any;

    public childIdent = 15;
    public nodeHeight = 30;

    public duration = 250;

    public tabulate() {
        var table = d3.select('#pane').append('table');
        var thead = table.append('thead');

        thead.append('tr')
            .selectAll('th')
            .data(Globals.columns).enter()
            .append('th')
            .text(function (column: any) { return column; });
    }

    public appendRows(data: any) {
        var table = d3.select('#pane').append('table');
        var tbody = table.append('tbody');

        var rows = tbody.selectAll('tr')
            .data(data)
            .enter()
            .append('tr')
            .attr("id", (d: any, i: any) => { return d.name; });

        // create a cell in each row for each column
        rows.selectAll('td')
            .data((row: any) => {
                return Globals.columns.map((column) => {
                    let val;

                    if (column === "Domain") {
                        val = row["rng"];
                    }

                    if (column === "Name") {
                        val = row["name"];
                    }

                    return { column: column, value: val };
                });
            })
            .enter()
            .append('td')
            .text((d: any) => { return d.value; });
    }


    public panel = jsPanel.create({
        // $("body").css( "background-color" );
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


    constructor() {
        this.createUL();
        this.tree = d3.layout.treelist()
            .childIndent(this.childIdent)
            .nodeHeight(this.nodeHeight);
    }


    public createUL() {
        this.init = true;
        this.ul = d3.select("#pane").append("ul").classed("treelist", "true");
    }
    public updatePanelTitle() {
        this.panel.setHeaderTitle("Node: " + State.selectedId);
    }

    public getSetPath(node: Node) {

        let path: string[] = [];

        let recurse = (node: Node) => {

            if (node.name !== "Children" && node.name !== "Items") {
                path.push(node.name);
            }

            if (node.parent) {
                if (node.parent.name === "Domain Variables") {
                    return node;
                }
                recurse(node.parent);
            }
        };

        recurse(node);
        return path.reverse().join(".");
    }

    public updateFromRoot() {
        this.update(this.id2Node["Items"]);
    }

    public static getVarName(node: Node): string {
        let name = node.name;
        if (node.parent && node.parent.children) {
            node.parent.children!.forEach((element: Node) => {
                if (element.name === "Cardinality") {
                    name = node.parent!.name + node.name;
                }
            });
        }
        return name;
    }

    public update(source: Node) {

        // console.log("in update");
        // console.log(source);

        var nodes = this.tree.nodes(source);

        var nodeEls = this.ul.selectAll("li.node")
            .data(nodes, (node: Node) => {
                node.id = node.id || this.nodeCount++;
                return node.id;
            });


        var entered = nodeEls.enter().append("li").classed("node", true)
            .attr("id", (node: Node) => {
                // let name = node.name;
                // if (node.parent) {
                //     node.parent.children!.forEach((element: Node) => {
                //         if (element.name === "Cardinality") {
                //             name = node.parent!.name + node.name;
                //         }
                //     });
                // }
                // node["domIdentifier"] = name;
                return Listview.getVarName(node);
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
            .style("height", this.tree.nodeHeight() + "px")
            .on("click", (d: any) => {

                // check if its a baby set
                // console.log(d._children)
                if (d._children) {
                    if (d._children.length === 0) {

                        let p = this.getSetPath(d);

                        State.pathList.push(p);

                        Globals.vscode.postMessage({
                            command: 'loadSet',
                            nodeId: State.selectedId,
                            path: p
                        });
                    }
                }

                Node.toggleNode(d);
                this.update(source);

            })
            .on("mouseover", (d: any) => {
                // d3.select(this).classed("changed", true);
            })
            .on("mouseout", (d: any) => {
                // d3.selectAll(".changed").classed("changed", false);
            })
            .each((node: Node) => {
                // this.id2Node[d.name] = d;
                this.id2Node[Listview.getVarName(node)] = node;
                if (node.name === "Expressions" && node.children && $("#expressions").prop("checked")) {
                    Node.toggleNode(node);
                }
            });
        //add arrows if it is a folder
        entered.append("span").attr("class", (d: any) => {
            var icon = d.children ? "fas fa-chevron-down"
                : d._children ? "fas fa-chevron-right" : "";
            return "caret " + icon;
        });

        entered.append("span").attr("class", "filename")
            .html((d: any) => { return d.name; });

        //  update the ranges

        d3.selectAll("span.filename").html((d: any) => {
            // console.log("D is  " + d.name);
            return d.name;
        });

        //update caret direction
        nodeEls.select("span.caret").attr("class", (d: any) => {
            var icon = d.children ? "fas fa-chevron-down"
                : d._children ? "fas fa-chevron-right" : "";
            return "caret " + icon;
        });
        //update position with transition
        nodeEls.transition().duration(this.duration)
            .style("top", (d: any) => { return (d.y - this.nodeHeight) + "px"; })
            .style("left", (d: any) => { return d.x + "px"; })
            .style("opacity", 1);
        nodeEls.exit().remove();
    }


    public setChangedExpressions(expressions: any) {
        // console.log(this.id2Node);
        // console.log(expressions);
        this.id2Node["Changed Expressions"]["children"] = expressions;
    }

    public updateNodes(data: any[]) {

        data.forEach((element: any) => {

            if (element.hasOwnProperty("Cardinality")) {
                if (this.id2Node[element.name].children) {

                    let setNode = this.id2Node[element.name].children;
                    // console.log(element)

                    setNode[1].children[0].name = element.Cardinality;

                    // if (setNode.children.length > 3){
                    if (element.Included) {

                        setNode[2].children[0].name = element["Not excluded"];
                        setNode[3].children[0].name = element.Included;

                        // let notExcluded = element["Not excluded"];
                        // let incList = element.Included;

                        // for (let i = 0; i < notExcluded.length; i++){
                        // if (incList.includes(notExcluded[i])){
                        // notExcluded[i] = "<b style='color:gold'>" + notExcluded[i] + "</b>";
                        // } 
                        // }

                        // setNode[2].children[0].name = notExcluded.join(", ");
                    }

                    if (element.Children && !setNode[2].children) {
                        setNode[2] = { name: "Children", children: element.Children.children }
                    }
                }
            }
            else {
                // console.log(element.name);
                this.id2Node[element.name].children[0].name = element.rng;
            }

        });

        this.updateFromRoot();
    }

    // public static setChangedList(list: any) {
    //     this.changedList = list;
    // }

    public setChanged(changedList: any[]) {

        // console.log(changedList)

        d3.selectAll("li").classed("changed", false);

        var ancestors: any = [];

        changedList.forEach((name: string) => {

            ancestors.push(name);

            // console.log("here");

            // console.log(name);
            // console.log(this.id2Node);

            if (this.id2Node[name]) {

                var obj = this.id2Node[name].parent;

                // console.log(obj);


                while (obj) {

                    // console.log(ancestors)
                    if (!ancestors.includes(obj.name)) {
                        ancestors.push(obj.name);
                    }
                    obj = obj.parent;
                }
            }


            // console.log(name);
            // console.log(ancestors);


            // console.log("---------");
            ancestors.forEach((id: string) => {
                // console.log(id);
                d3.select('[id="' + $.escapeSelector(id) + '"]').classed("changed", true);
                // $($.escapeSelector( "#" + id)).attr("class","changed");
            });

        });
    }

}

