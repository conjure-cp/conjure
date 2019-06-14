import "../util/treelist";
import Globals from "./Globals";
import * as Web from "./Web";
import State from "./State";
import Node from "./Node";

declare var jsPanel: any;
declare var d3: any;

/**
 * This class handles the panel that contains the information on the domains
 */

export default class Panel {
  public nodeCount = 0;
  public id2Node: { [id: string]: any } = {};
  public tree: any;
  public ul: any;
  public childIdent = 15;
  public nodeHeight = 30;
  public duration = 0;
  public columns = ["Name", "Domain"];

  // The panel to house the data on domains.
  public panel = jsPanel.create({
    onbeforeclose: function(panel: any, status: any) {
      return false;
    },
    theme:
      getComputedStyle(document.documentElement).getPropertyValue(
        "--vscode-editor-background"
      ) + " filled",
    headerTitle: "my panel #1",
    position: "500 0 0 0",
    contentSize: {
      width: () => {
        return $(document).width()! / 2.5;
      },
      height: () => {
        return $(document).height()! * 0.8;
      }
    },
    content: `
        <div id="pane"> </div>`
  });

  constructor() {
    this.createUL();
    this.tree = d3.layout
      .treelist()
      .childIndent(this.childIdent)
      .nodeHeight(this.nodeHeight);
  }

  /**
   * Creates an unordered list element and appends it to the jsPanel.
   */
  public createUL() {
    this.ul = d3
      .select("#pane")
      .append("ul")
      .classed("treelist", "true");
  }

  /**
   * Creates a table for displaying simple domains.
   */
  public tabulate() {
    // Append the table element.
    var table = d3.select("#pane").append("table");
    // Append the header tag.
    var thead = table.append("thead");
    // Add the columns to the header.
    thead
      .append("tr")
      .selectAll("th")
      .data(this.columns)
      .enter()
      .append("th")
      .text(function(column: any) {
        return column;
      });
  }

  /**
   * Append rows to the simple domains table.
   * @param data The list of rows to be appended.
   */
  public appendRows(data: any) {
    var table = d3.select("#pane table");
    var tbody = table.append("tbody");

    var rows = tbody
      .selectAll("tr")
      .data(data)
      .enter()
      .append("tr")
      .attr("id", (d: any, i: any) => {
        return d.name;
      });

    // create a cell in each row for each column
    rows
      .selectAll("td")
      .data((row: any) => {
        return this.columns.map(column => {
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
      .append("td")
      .text((d: any) => {
        return d.value;
      });
  }

  /**
   * Updates the title of the panel.
   */
  public updatePanelTitle() {
    this.panel.setHeaderTitle("Node: " + State.selectedId);
  }

  /**
   * Gets the path of a nested set in the tree list.
   * @param node The node representing the set.
   */
  public getSetPath(node: Node) {
    let path: string[] = [];

    // Recurse up until we get to the root node.
    let recurse = (node: Node) => {
      // Don't want to include these names in the path.
      if (node.name !== "Children" && node.name !== "Items") {
        // Include the names of intermediate sets.
        path.push(node.name);
      }
      // If the parent is the root then we are at the outemost set.
      if (node.parent) {
        if (node.parent.name === "Domain Variables") {
          return node;
        }
        recurse(node.parent);
      }
    };
    recurse(node);

    // Build a string representation of the path.
    return path.reverse().join(".");
  }

  /**
   * Update the treelist starting at the root.
   */
  public updateFromRoot() {
    this.update(this.id2Node["Items"]);
  }

  /**
   * Return the variable name.
   * If the variable is a set then prepend its name with its parent's name.
   * @param node node in the tree list.
   */
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

  /**
   * Update the tree list.
   * @param source The node to start the upadte from
   */
  public update(source: Node) {
    var nodeList = this.tree.nodes(source);

    // Bind the dom elements to the nodes
    var nodeDomElements = this.ul
      .selectAll("li.node")
      .data(nodeList, (node: Node) => {
        node.id = node.id || this.nodeCount++;
        return node.id;
      });

    // Enter the nodes
    var entered = nodeDomElements
      .enter()
      // Append list item
      .append("li")
      // Set the class to node.
      .classed("node", true)
      // Set the selector id to the name of the variable
      .attr("id", (node: Node) => {
        return Panel.getVarName(node);
      })
      // Work out the y coordinate of the node by using its parent
      .style("top", (node: Node) => {
        let suffix = "px";
        if (source.parent) {
          return source.parent.y + suffix;
        }

        return node.y + suffix;

        // return source.y + suffix;
      })

      .style("opacity", 0)
      .style("height", this.tree.nodeHeight() + "px")

      // Register click event handler.

      .on("click", (node: Node) => {
        // If the node has the _children list but its empty
        // then that node must be a set with child sets that are not yet loaded.
        if (node._children) {
          if (node._children.length === 0) {
            // Get the path of the set.
            let path = this.getSetPath(node);

            // Add the path to the list of paths wanted.
            if (!State.pathList.includes(path)) {
              State.pathList.push(path);
            }

            // Load the set with that path
            // Globals.vscode.postMessage({
            //     command: 'loadSet',
            //     nodeId: State.selectedId,
            //     path: path
            // });

            Web.getRequest("loadSet/" + State.selectedId + "/" + path)
              .then(response => {
                let data = JSON.parse(response);
                console.log(data);
                // Add the child set to the parent
                Globals.lv.id2Node[data.structure.name].children =
                  data.structure.children;
                // Update the tree list
                Globals.lv.updateFromRoot();
                // Update the values of the domains
                Globals.lv.updateDomains([data.update]);
                // Send a pretty request to get changes
                Globals.sendPrettyRequest(Globals.vscode);
              })
              .catch(error => {
                console.error(error);
              });
          }
        }
        // Toggle the node and update the tree list.
        Node.toggleNode(node);
        this.update(source);
      })
      .each((node: Node) => {
        // add each node to the map
        this.id2Node[Panel.getVarName(node)] = node;
        // If freeze expressions is ticked then collapse the expressions node.
        if (
          node.name === "Expressions" &&
          node.children &&
          $("#expressions").prop("checked")
        ) {
          Node.toggleNode(node);
        }
      });

    // Add chevrons
    entered.append("span").attr("class", (node: Node) => {
      // If the node has expanded children then add down chevrons
      var icon = node.children
        ? "fas fa-chevron-down"
        : // If the node has collapsed children then add right chevrons
        node._children
        ? "fas fa-chevron-right"
        : "";
      return "caret " + icon;
    });

    // Add the name of the node.
    entered
      .append("span")
      .attr("class", "filename")
      .html((d: any) => {
        return d.name;
      });

    d3.selectAll("span.filename").html((d: any) => {
      return d.name;
    });

    //update chevron direction
    nodeDomElements.select("span.caret").attr("class", (node: Node) => {
      var icon = node.children
        ? "fas fa-chevron-down"
        : node._children
        ? "fas fa-chevron-right"
        : "";
      return "caret " + icon;
    });

    //update position with transition
    nodeDomElements
      .transition()
      .duration(this.duration)
      .style("top", (d: any) => {
        return d.y - this.nodeHeight + "px";
      })
      .style("left", (d: any) => {
        return d.x + "px";
      })
      .style("opacity", 1);
    nodeDomElements.exit().remove();
  }

  /**
   * Set the changed expressions node
   * @param expressions The list of expressions that have changed since the previous node.
   */

  public setChangedExpressions(expressions: any) {
    this.id2Node["Changed Expressions"]["children"] = expressions;
  }

  public updateSet(old: any, n: any) {
    old.Cardinality = n.Cardinality;
    old.Children = n.Children;
    old.Included = n.Included;
    old["Not excluded"] = n["not excluded"];
  }

  /**
   * Update the domains of variables
   * @param data The list of domains
   */
  public updateDomains(data: any[]) {
    // let names: string[] = [];

    // data.forEach((item:any) => {
    //     names.push(item.name);
    // });

    // Object.keys(this.id2Node).forEach((name: string)=> {
    //     if (!names.includes(name) && this.id2Node[name].Cardinality){
    //         console.log("remove " + name);
    //     }
    // })

    data.forEach((element: any) => {
      // Check if the domain is a set
      if (element.hasOwnProperty("Cardinality")) {
        if (this.id2Node[element.name].children) {
          let setNode = this.id2Node[element.name].children;
          // Update the cardinality
          if (setNode[1].children) {
            setNode[1].children[0].name = element.Cardinality;
          }
          // Update the included and Not excluded
          if (element.Included) {
            if (setNode[2].children) {
              setNode[2].children[0].name = element["Not excluded"];
            }

            if (setNode[3].children) {
              setNode[3].children[0].name = element.Included;
            }
          } else {
            if (!setNode[2].children) {
              setNode[2].children = [];
              //     setNode[2] = { name: "Children", children: [] };
            }
            // }
            let updatedNames: string[] = [];

            if (element.Children.children) {
              let currentNames: string[] = [];

              setNode[2].children.forEach((child: any) => {
                currentNames.push(child.name);
              });

              element.Children.children.forEach((child: any) => {
                updatedNames.push(child.name);

                if (!currentNames.includes(child.name)) {
                  setNode[2].children.push(child);
                }
              });
            }

            for (let i = 0; i < setNode[2].children.length; i++) {
              let child = setNode[2].children[i];

              if (!updatedNames.includes(child.name)) {
                setNode[2].children.splice(i, 1);
              }
            }
          }
          // Append child sets
          // if (element.Children && !setNode[2].children) {
          // if (element.Children) {
          // }
        }
      }
      // If not set then just update the range
      else {
        this.id2Node[element.name].children[0].name = element.rng;
      }
    });
    this.updateFromRoot();
  }

  /**
   * Sets the class of nodes that have changed to "changed"
   * @param changedList list of names of domains that have changed
   */
  public setChanged(changedList: string[]) {
    // Remove the class from all nodes.
    d3.selectAll("li").classed("changed", false);

    // List of all ancestor names
    var ancestors: any = [];

    changedList.forEach((name: string) => {
      ancestors.push(name);

      if (this.id2Node[name]) {
        var obj = this.id2Node[name].parent;

        // Go through each ancestor until we get to the root.

        while (obj) {
          if (!ancestors.includes(obj.name)) {
            ancestors.push(obj.name);
          }
          obj = obj.parent;
        }
      }

      // Set the class of all ancestors to "changed"

      ancestors.forEach((id: string) => {
        d3.select('[id="' + $.escapeSelector(id) + '"]').classed(
          "changed",
          true
        );
      });
    });
  }
}
