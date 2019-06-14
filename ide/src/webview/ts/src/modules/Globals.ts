import State from "./State";
import Panel from "./Listview";
import Listener from "./Listener";
import * as Web from "./Web";
import Tree from "./Tree";
import Node from "./Node";

// declare var acquireVsCodeApi: any;
declare var d3: any;

/**
 * This class is used to interact with the vscode api to send messages to the extension
 */
export default class Globals {
  public static lv = new Panel();
  public static vscode = "";

  /**
   * Requests to load the child nodes of the selected node.
   * @param vscodeApi
   */
  public static loadNodes(vscodeApi: any) {
    if (!State.waiting) {
      // vscodeApi.postMessage({
      //     command: 'loadNodes',
      //     amount: 1,
      //     start: State.selectedId
      // });

      Web.getRequest("loadNodes/" + State.selectedId)
        .then(response => {
          let data = JSON.parse(response);
          // console.log(data);
          data.forEach((element: any) => {
            // If we don't already have this node then add it to the tree
            if (!State.id2Node[element.id]) {
              State.addNode(element.parentId, element);
              State.totalLoaded++;
            }
          });

          Tree.update(State.id2Node[State.rootId]);
          Tree.selectNode(State.selectedId);
          Listener.setLoadedCount();
          State.waiting = false;
        })
        .catch(error => {
          console.error(error);
        });

      State.waiting = true;
    }
  }

  /**
   * Sends the appropriate request to load the domains`
   * @param vscodeApi
   */
  public static loadDomains(vscodeApi: any) {
    if (State.pretty) {
      // Globals.sendPrettyRequest(vscodeApi);
      Web.getRequest(
        "prettyDomains/" +
          State.selectedId +
          "/" +
          !$("#expressions").prop("checked") +
          "/" +
          State.pathList.join(":")
      )
        .then(response => {
          let data = JSON.parse(response);
          // console.log(data);
          Globals.lv.setChangedExpressions(data.changedExpressions);
          Globals.lv.updateDomains(data.vars);
          Globals.lv.setChanged(data.changed);
          Globals.lv.updateFromRoot();
        })
        .catch(error => {
          console.error(error);
        });
    } else {
      Web.getRequest(
        "simpleDomains/" +
          State.selectedId +
          "/" +
          !$("#expressions").prop("checked")
      )
        .then(response => {
          let data = JSON.parse(response);
          console.log(data);
          data.vars.forEach((variable: any) => {
            // Reset changed status for all simple domains.
            $("#" + $.escapeSelector(variable.name)).removeClass("changed");
            let li = $(
              "#" + $.escapeSelector(variable.name) + " > :last-child"
            );
            // Update the range for each domain.
            li.text(variable.rng);
            // Update which domains have changed.
            if (data.changedNames.includes(variable.name)) {
              d3.select(
                '[id="' + $.escapeSelector(variable.name) + '"]'
              ).classed("changed", true);
            }
          });
        })
        .catch(error => {
          console.error(error);
        });
      // Globals.sendSimpleRequest(vscodeApi);
    }
  }

  /**
   * Sends a request for simple domains.
   * @param vscodeApi
   */
  public static sendSimpleRequest(vscodeApi: any) {
    vscodeApi.postMessage({
      command: "simpleDomains",
      nodeId: State.selectedId,
      wantExpressions: !$("#expressions").prop("checked")
    });
  }

  /**
   * Sends a request for pretty domains.
   * @param vscodeApi
   */
  public static sendPrettyRequest(vscodeApi: any) {
    vscodeApi.postMessage({
      command: "prettyDomains",
      nodeId: State.selectedId,
      wantExpressions: !$("#expressions").prop("checked"),
      paths: State.pathList.join(":")
    });
  }

  /**
   * Sends requests on initialisation
   * @param vscodeApi
   */
  public static initialize(vscodeApi: any) {
    // vscodeApi.postMessage({
    //     command: 'init',
    // });

    Web.getRequest(
      "init//home/tom/SearchTreeVisualisationTests/testData/gears1000/"
    )
      .then(response => {
        let data = JSON.parse(response);
        $("#configuration").text(data.config);

        // Loads the tree list at the root.
        Globals.lv.update(data.prettyAtRoot);
        // Stores the simple domains at the root
        State.simpleDomainsAtRoot = data.simpleAtRoot.vars;
        // Stores the ids of solution ancestors
        State.solAncestorIds = data.core.solAncestorIds;
        // Increases the number of totalLoaded
        State.totalLoaded += data.core.nodes.length;

        // Add each node from the core to the tree.
        for (let i = 0; i < data.core.nodes.length; i++) {
          let element = data.core.nodes[i];
          State.addNode(element.parentId, element);
        }

        // Collapse all the nodes so that only the root node appears on startup.
        Node.collapseNode(State.id2Node[State.rootId]);
        Tree.update(State.id2Node[State.rootId]);
        Tree.selectNode(State.rootId);
        Listener.setLoadedCount();
        Listener.setSolCount();

        console.log(data);
      })
      .catch(error => {
        console.error(error);
      });

    Web.getRequest("longestBranchingVariable")
      .then(response => {
        response = Number(response);
        // console.log(response);
        Tree.tree.nodeSize([Number(response) * 13, Tree.nodeHeight]);
      })
      .catch(error => {
        console.error(error);
      });

    // vscodeApi.postMessage({
    //     command: 'longestBranchingVariable',
    // });
  }
}
