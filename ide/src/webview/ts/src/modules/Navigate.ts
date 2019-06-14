import State from "./State";
import Node from "./Node";
import Globals from "./Globals";

/**
 * This class handles navigation through the tree
 */

export default class Navigate {
  /**
   * Move to the next node (search order)
   * @param vscodeApi Vscode api object
   */

  public static nextNode(vscodeApi: any) {
    let stepSize = 1;
    let node = State.id2Node[State.selectedId];

    if (node._children) {
      Node.toggleNode(node);
      return;
    }

    if (!State.id2Node[State.selectedId + stepSize]) {
      Globals.loadNodes(vscodeApi);
    } else {
      State.selectedId += stepSize;
    }
  }

  /**
   * Move to the previous node (search order)
   */

  public static previousNode() {
    let prevId = State.selectedId - 1;

    if (State.id2Node[prevId]) {
      State.selectedId--;
    }
  }

  /**
   * Take the right branch at a node with a left and a right child.
   */

  public static rightNode() {
    let node = State.id2Node[State.selectedId];
    if (node.children) {
      if (node.children.length > 1) {
        State.selectedId = node.children![1].id;
      }
    }
  }

  /**
   * Take the left branch at a node with a left and a right child.
   */

  public static leftNode() {
    let node = State.id2Node[State.selectedId];
    if (node.children) {
      if (node.children.length > 1) {
        State.selectedId = node.children[0].id;
      }
    }
  }

  /**
   * Move to the parent of the currently selected node.
   */

  public static upNode() {
    if (State.selectedId > State.rootId) {
      State.selectedId = State.id2Node[State.selectedId].parent!.id;
    }
  }

  /**
   * Cycle to the next solution node (search order).
   */

  public static nextSolutionNode() {
    if (State.solNodIds.length === 0) {
      return;
    }

    if (!State.solNodIds.includes(State.selectedId)) {
      State.selectedId = State.solNodIds[0];
      return;
    }

    let currentSolId = State.solNodIds.indexOf(State.selectedId);

    if (currentSolId + 1 < State.solNodIds.length) {
      State.selectedId = State.solNodIds[currentSolId + 1];
    }
  }

  /**
   * Cycle to the previous solution node (search order)
   */

  public static previousSolutionNode() {
    if (State.solNodIds.length === 0) {
      return;
    }

    if (!State.solNodIds.includes(State.selectedId)) {
      State.selectedId = State.solNodIds[State.solNodIds.length - 1];
      return;
    }

    let currentSolId = State.solNodIds.indexOf(State.selectedId);

    if (currentSolId - 1 >= 0) {
      State.selectedId = State.solNodIds[currentSolId - 1];
    }
  }
}
