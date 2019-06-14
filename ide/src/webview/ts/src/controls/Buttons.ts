import Img from "../util/exportImage";
import * as legend from "../modules/Legend";
import Globals from "../modules/Globals";
import Tree from "../modules/Tree";
import State from "../modules/State";
// import State from '../modules/State';

declare var d3: any;

/**
 * This class binds on screen to event handlers.
 */
export default class Buttons {
  /**
   * Handles when the "pretty labels" checkbox is clicked
   */

  public static handleLabels() {
    // Select all nodes

    let domObjects = $("g.node");

    for (var i = 0; i < domObjects.length; i++) {
      let id = domObjects[i].id;
      // If checked then set text to pretty
      if ($("#labels").prop("checked") === true) {
        $("#" + id + " text").text(
          State.id2Node[Number(id.replace("node", ""))].prettyLabel
        );
      } else {
        // Otherwise set text to simple
        $("#" + id + " text").text(
          State.id2Node[Number(id.replace("node", ""))].label
        );
      }
    }
    Tree.update(State.id2Node[State.rootId]);
  }

  /**
   * Handles when the "pretty Domains" check box is clicked
   */
  public static handleDomainsCheckbox() {
    // clear the panel.
    $("#pane").empty();
    State.pretty = !State.pretty;

    // Add the tree list
    if (State.pretty) {
      Globals.lv.createUL();
      Globals.lv.update(Globals.lv.id2Node["Items"]);
    } else {
      // Add the simple domains

      if (!State.simpleDomainsAtRoot) {
        Globals.loadDomains(Globals.vscode);
      } else {
        Globals.lv.tabulate();
        Globals.lv.appendRows(State.simpleDomainsAtRoot);
      }
    }

    Tree.selectNode(State.selectedId);
  }

  /**
   * Binds buttons to their handlers
   */
  public static bindButtons() {
    d3.select("#legend").on("click", legend.showLegend);

    d3.select("#saveImg").on("click", Img.writeImage);

    d3.select("#freeze").on("change", () => {
      Globals.loadDomains(Globals.vscode);
      State.frozen = !State.frozen;
    });

    d3.select("#expressions").on("change", () => {
      Globals.loadDomains(Globals.vscode);
    });

    d3.select("#labels").on("change", this.handleLabels);

    d3.select("#check").on("change", Buttons.handleDomainsCheckbox);
  }
}
