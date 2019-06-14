declare var d3: any;
import Globals from "./Globals";
import Tree from "./Tree";
import Node from "./Node";
import State from "./State";

/**
 * Class that handles incoming messages from the vscode extension
 */
export default class Listener {
  /**
   * Set the total number of nodes loaded.
   */
  public static setLoadedCount() {
    $("#total").text(
      State.totalLoaded +
        "/" +
        Number(State.id2Node[State.rootId].descCount + 1)
    );
  }

  public static setSolCount() {
    $("#solutions").text(State.solNodIds.length);
  }

  /**
   * Handles the init message
   * @param data the init response
   */
  // public static initHandler(data: any) {

  // }

  /**
   * Sets the width of the node corresponding to the longest label.
   * @param length The length of the largest branching variable
   */
  public static longestBranchingVariableHandler(length: number) {
    Tree.tree.nodeSize([Number(length) * 13, Tree.nodeHeight]);
  }

  /**
   * Handles incoming new nodes.
   * @param data List of new nodes
   */
  // public static loadNodesHandler(data: any) {

  //     data.forEach((element: any) => {
  //         // If we don't already have this node then add it to the tree
  //         if (!State.id2Node[element.id]) {
  //             State.addNode(element.parentId, element);
  //             State.totalLoaded++;
  //         }
  //     });

  //     Tree.update(State.id2Node[State.rootId]);
  //     Tree.selectNode(State.selectedId);
  //     Listener.setLoadedCount();
  // }

  /**
   * Handles incoming set
   * @param data the set
   */
  // public static loadSetHandler(data: any) {
  //     // Add the child set to the parent
  //     Globals.lv.id2Node[data.structure.name].children = data.structure.children;
  //     // Update the tree list
  //     Globals.lv.updateFromRoot();
  //     // Update the values of the domains
  //     Globals.lv.updateDomains([data.update]);
  //     // Send a pretty request to get changes
  //     Globals.sendPrettyRequest(Globals.vscode);
  // }

  /**
   * Handles incoming simple domains.
   * @param data List of simple domains.
   */
  // public static simpleDomainsHandler(data: any) {
  //     data.vars.forEach((variable: any) => {
  //         // Reset changed status for all simple domains.
  //         $("#" + $.escapeSelector(variable.name)).removeClass("changed");
  //         let li = $("#" + $.escapeSelector(variable.name) + " > :last-child");
  //         // Update the range for each domain.
  //         li.text(variable.rng);
  //         // Update which domains have changed.
  //         if (data.changedNames.includes(variable.name)) {
  //             d3.select('[id="' + $.escapeSelector(variable.name) + '"]').classed("changed", true);
  //         }
  //     });
  // }

  /**
   * Handle incoming pretty domains
   * @param data list of pretty domains
   */
  // public static prettyDomainsHandler(data: any){
  //     // console.log(data)
  //     Globals.lv.setChangedExpressions(data.changedExpressions);
  //     Globals.lv.updateDomains(data.vars);
  //     Globals.lv.setChanged(data.changed);
  //     Globals.lv.updateFromRoot();
  // }

  /**
   * Bind handlers to listeners
   */
  public static bindListener() {
    window.addEventListener("message", event => {
      const message = event.data;
      switch (message.command) {
        case "loadSet":
          // Listener.loadSetHandler(message.data);
          break;

        case "init":
          // Listener.initHandler(message.data);
          break;

        case "longestBranchingVariable":
          // Listener.longestBranchingVariableHandler(message.data);
          break;

        case "loadNodes":
          // Listener.loadNodesHandler(message.data);
          State.waiting = false;
          break;

        case "simpleDomains":
          // Listener.simpleDomainsHandler(message.data);
          break;

        case "prettyDomains":
          // Listener.prettyDomainsHandler(message.data);
          break;
      }
    });
  }
}
