declare var d3: any;
import Globals from "./Globals";
import State from "./State";
import Node from "./Node";

/**
 * This class represents the graphical state of the tree.
 */

export default class Tree {
  public static duration = 500;
  public static viewerWidth = $(document).width();
  public static viewerHeight = $(document).height();
  public static margin = { top: 40, right: 30, bottom: 50, left: 30 };
  public static width =
    Tree.viewerWidth! - Tree.margin.left - Tree.margin.right;
  public static height =
    Tree.viewerHeight! - Tree.margin.top - Tree.margin.bottom;
  public static nodeHeight = 150;
  public static tree = d3.layout
    .tree()
    .size([Tree.height, Tree.width])
    .nodeSize([200, 0]);
  public static zoom = d3.behavior.zoom().on("zoom", Tree.zoomed);
  public static svg = Tree.makeSvg(
    "#tree",
    Tree.zoom,
    Tree.viewerWidth,
    Tree.viewerHeight
  );

  public static diagonal = d3.svg.diagonal().projection((d: any) => {
    return [d.x, d.y];
  });

  /**
   * Creates the main svg container which the rest of the components will reside in.
   * @param selector The html element to which the svg container will be appended
   * @param zoomFunc The function used to provide zoom and panning transformations.
   * @param width    The width of the svg.
   * @param height   The height of the svg.
   */

  public static makeSvg(
    selector: string,
    zoomFunc: any,
    width: number,
    height: number
  ) {
    return d3
      .select(selector)
      .append("svg")
      .call(zoomFunc)
      .attr("id", "theTree")
      .attr("width", width)
      .attr("height", height)
      .append("g");
  }

  /**
   * Method specifying the transformation on zoom/pan events.
   */
  public static zoomed() {
    Tree.svg.attr(
      "transform",
      "translate(" + d3.event.translate + ")" + " scale(" + d3.event.scale + ")"
    );
  }

  /**
   * Focusses the camera to the given node.
   * @param node The node to be focussed on.
   */

  public static focusNode(node: Node) {
    let scale = Tree.zoom.scale();
    let x = -node.x * scale;
    let y = -node.y * scale;

    x += Tree.width / 3;
    y += Tree.height / 2;

    d3.select("g")
      .transition()
      .duration(Tree.duration)
      .attr("transform", "translate(" + x + "," + y + ")scale(" + scale + ")");
    Tree.zoom.translate([x, y]);
  }

  /**
   * Changes the currently selected node.
   * Removes the "selected" css class from the old nodes and adds it to the new node.
   * The selected node is focussed on.
   * The domains of the selected node are loaded.
   * @param nodeId
   */

  public static selectNode(nodeId: number) {
    State.selectedId = nodeId;

    let allCircles = ".node circle";
    d3.selectAll(allCircles).classed("selected", false);
    let s = "#node" + nodeId + " circle";

    d3.select(s).classed("selected", true);
    Tree.focusNode(State.id2Node[nodeId]);

    if (!State.frozen) {
      Globals.loadDomains(Globals.vscode);
    }

    Globals.lv.updatePanelTitle();
  }

  /**
   * Append an SCG circle to the DOM
   * @param selection the DOM object to append the circle to.
   */
  public static appendCircle(selection: any) {
    selection.append("circle").attr("r", (node: Node) => {
      return Node.calculateRadius(node);
    });
  }

  /**
   * Sets the css class corresponding to the filled colour of the circle.
   * Solution nodes are filled gold.
   * Nodes in failed branches that have children that are not visible are filled red.
   * Nodes in solution branches and have children that are not visible are filled green.
   * Nodes that are not solutions nodes and do not have any children that are not visible are filled white.
   * @param node
   */
  public static fillCircle(node: Node) {
    let s = "#node" + node.id + " circle";

    let domElement = d3.select(s);
    domElement.classed("hasOthers red", false);

    if (node.isSolution) {
      domElement.classed("solution", true);
    }

    if (Node.hasMoreChildren(node)) {
      if (
        State.solAncestorIds.includes(node.id) &&
        State.solNodIds.length > 0
      ) {
        domElement.classed("hasOthers", true);
      } else {
        domElement.classed("hasOthers red", true);
      }
    }
  }

  /**
   * Flattens the nested structure into a list of nodes.
   * The position of each node is calculated and the x and y attributes of each node are updated.
   * Each node is bound to the DOM object representing it.
   * @param node The root node.
   */
  public static getNodeList(node: Node) {
    let nodeList = Tree.tree.nodes(node);
    nodeList.forEach((node: Node) => {
      node.y = node.depth * Tree.nodeHeight;
    });
    return nodeList;
  }

  /**
   * Append a <g> element for every node in the tree.
   * @param selection The DOM object to append the nodes to
   */
  public static enterNodes(selection: any) {
    return (
      selection
        .enter()
        .append("g")
        // set css class
        .attr("class", "node")
        // Set the id of the dom object to the node id
        .attr("id", (node: Node) => {
          return "node" + node.id;
        })
        // Set initial position
        // Each node initially originates from its parent and then transitions to its own position.
        .attr("transform", (node: Node) => {
          let parent = node.parent;
          if (parent) {
            return "translate(" + parent.x + "," + parent.y + ")";
          }
          // The root node's position will start at (0,0)
        })
        // Register click events to handler.
        .on("click", (node: Node) => {
          Tree.selectNode(node.id);
        })
        // Add each node to the id2Node map.
        .each((d: Node) => {
          if (!State.id2Node[d.id]) {
            State.id2Node[d.id] = d;
          }
        })
    );
  }

  /**
   *  Append the branching condition labels to each link.
   * @param selection The DOM object to append the label to.
   */

  public static appendLabel(selection: any) {
    selection
      .append("text")
      // Labels y coordinate should be halfway between the node and its parent.
      .attr("y", () => {
        return -Tree.nodeHeight / 2;
      })
      .attr("dy", ".35em")
      .attr("text-anchor", "middle")
      // Set the text
      .text((node: Node) => {
        // Check the labels checkbox to see if pretty or simple labels are needed.
        if ($("#labels").is(":checked")) {
          return node.prettyLabel;
        } else {
          return node.label;
        }
      })
      .style("fill-opacity", 1e-6);
  }

  /**
   * Appends the label displaying the number of descendants a collapsed node has.
   * @param selection  Dom object to append the label to
   */
  public static appendDescCount(selection: any) {
    selection
      .append("text")
      .attr("y", (node: Node) => {
        return Node.getDescLabelHeight(node);
      })
      .attr("class", "decCount")
      .attr("dy", ".35em")
      .attr("text-anchor", "middle")
      .text((node: Node) => {
        if (Node.hasMoreChildren(node)) {
          return node.descCount + " nodes below";
        }
      });
  }

  /**
   * Update the location of a node and return the transition object.
   * @param selection The DOM object representing the node.
   */
  public static updateLocation(selection: any) {
    return selection
      .transition()
      .duration(Tree.duration)
      .attr("transform", (node: Node) => {
        return "translate(" + node.x + "," + node.y + ")";
      });
  }

  /**
   * Update the radius of the circle and its fill colour
   * @param selection The DOM object representing the node
   */
  public static updateCircle(selection: any) {
    selection
      .select("circle")
      .attr("r", (node: Node) => {
        let newRadius = Node.calculateRadius(node);
        return newRadius;
      })
      .each((d: Node) => {
        Tree.fillCircle(d);
      });
  }

  /**
   * Update the text for the desc count and branching variable labels
   * @param selection The DOM object representing the node.
   */
  public static updateText(selection: any) {
    selection.select("text").style("fill-opacity", 1);
    selection
      .select("text.decCount")
      .attr("y", (node: Node) => {
        return Node.getDescLabelHeight(node);
      })
      .text((node: Node) => {
        if (Node.hasMoreChildren(node)) {
          return node.descCount + " Nodes below";
        }
      });
  }

  /**
   * Removes nodes from the DOM
   * @param selection The DOM object representing the nodes.
   * @param source The source of the root node.
   */

  public static exitNodes(selection: any, source: Node) {
    return (
      selection
        .exit()
        .transition()
        .duration(Tree.duration)
        // When a node is collapsed the child nodes move to the position of their parent.
        .attr("transform", (node: Node) => {
          let parent = node.parent;
          if (parent) {
            return "translate(" + parent.x + "," + parent.y + ")";
          }
          return "translate(" + source.x + "," + source.y + ")";
        })
        // After the transition remove the node completely.
        .remove()
    );
  }

  /**
   * Returns a list of links between nodes.
   * @param nodeList List of nodes.
   */
  public static getLinkList(nodeList: Node[]) {
    return Tree.tree.links(nodeList);
  }

  /**
   * Append the link svgs to the DOM and returns the link selection.
   * @param linkDomObjects The DOM objects to append the links to.
   */
  public static enterLinks(linkDomObjects: any) {
    return (
      linkDomObjects
        .enter()
        .insert("path", "g")
        .attr("class", (link: any) => {
          // If the link's target node is in a solution branch then it will be green.
          if (
            State.solAncestorIds.includes(link.target.id) &&
            State.solNodIds.length > 0
          ) {
            return "link";
          }
          // If not then the link will be red.
          return "link red";
        })
        // Draw the path of the link as an svg path
        .attr("d", (d: any) => {
          let o = { x: d.source.x, y: d.source.y };
          return Tree.diagonal({ source: o, target: o });
        })
        .style("stroke-opacity", 1e-6)
    );
  }

  /**
   * Update the visual graphical representation of the tree.
   * @param source The node from which the update will be started.
   */

  public static update(source: Node) {
    let nodeList = Tree.getNodeList(State.id2Node[State.rootId]);
    let linkList = Tree.getLinkList(nodeList);
    // Bind the nodes to their dom objects.
    let nodeDomObjects = Tree.svg
      .selectAll("g.node")
      .data(nodeList, (node: Node) => {
        return node.id;
      });
    // Append dom objects.
    let nodeEnter = Tree.enterNodes(nodeDomObjects);
    Tree.appendCircle(nodeEnter);
    Tree.appendLabel(nodeEnter);
    Tree.appendDescCount(nodeEnter);

    // Update nodes>
    let nodeUpdate = Tree.updateLocation(nodeDomObjects);
    Tree.updateCircle(nodeUpdate);
    Tree.updateText(nodeUpdate);
    // Remove node DOM objects.
    let nodeExit = Tree.exitNodes(nodeDomObjects, source);
    nodeExit.select("text").style("fill-opacity", 1e-6);

    // Bind the links to their dom objects.
    let linkDomObjects = Tree.svg
      .selectAll("path.link")
      .data(linkList, (link: any) => {
        return link.target.id;
      });
    // Append the link DOM objects
    Tree.enterLinks(linkDomObjects);
    linkDomObjects
      .transition()
      .duration(Tree.duration)
      .attr("d", Tree.diagonal)
      .style("stroke-opacity", 1);
    // Remove the link DOM objects
    linkDomObjects
      .exit()
      .transition()
      .duration(Tree.duration)
      .attr("d", (d: any) => {
        let o = { x: d.source.x, y: d.source.y };
        return Tree.diagonal({ source: o, target: o });
      })
      .remove();
  }
}
