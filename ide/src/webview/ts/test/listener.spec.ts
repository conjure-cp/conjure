import Listener from "../src/modules/Listener";
import State from "../src/modules/State";
import Tree from "../src/modules/Tree";

declare var d3: any;

describe("Test the Listener class", function() {
  beforeEach(function() {
    State.id2Node = {};
    State.solAncestorIds = [];
    State.solNodIds = [];
    State.rootId = 0;
    State.selectedId = State.rootId;
    State.waiting = false;
    State.pretty = true;
    State.totalLoaded = 0;

    $("body").empty();
  });

  describe("Init handler ", function() {
    // let node1 = ;
    let core = {
      nodes: [
        {
          childCount: 1,
          decCount: 36,
          id: 0,
          isLeftChild: true,
          isSolution: false,
          label: "",
          parentId: -1,
          prettyLabel: ""
        },
        {
          childCount: 2,
          decCount: 35,
          id: 1,
          isLeftChild: true,
          isSolution: false,
          label: "Root Propagation",
          parentId: 0,
          prettyLabel: "Root Propagation"
        }
      ],
      solAncestorIds: [1, 2, 3]
    };

    let fakeResponse = {
      simpleAtRoot: { vars: { name: "x", rng: "int(1..10)" } },
      prettyAtRoot: { blah: "blah" },
      core: core
    };

    it("Should updateTheState", function() {
      // Listener.initHandler(fakeResponse);
      expect(State.solAncestorIds).toBe(core.solAncestorIds);
      expect(State.simpleDomainsAtRoot).toBe(fakeResponse.simpleAtRoot.vars);

      core.nodes.forEach((n: any) => {
        expect(State.id2Node[n.id]).not.toBeNull();
      });

      expect(State.totalLoaded).toEqual(core.nodes.length);
    });
  });

  describe("longest branching variable", function() {
    // let node1 = ;
    it("Should update the node size of the tree", function() {
      Listener.longestBranchingVariableHandler(10);
      expect(Tree.tree.nodeSize()).toEqual([130, 150]);
    });
  });

  describe("loadNodes", function() {
    it("Should update the node size of the tree", function() {
      let nodes = [
        {
          childCount: 1,
          decCount: 36,
          id: 0,
          isLeftChild: true,
          isSolution: false,
          label: "",
          parentId: -1,
          prettyLabel: ""
        },
        {
          childCount: 2,
          decCount: 35,
          id: 1,
          isLeftChild: true,
          isSolution: false,
          label: "Root Propagation",
          parentId: 0,
          prettyLabel: "Root Propagation"
        }
      ];

      // Listener.loadNodesHandler(nodes);

      expect(State.id2Node[0]).not.toBeNull();
      expect(State.id2Node[1]).not.toBeNull();
      expect(State.totalLoaded).toBe(2);
    });
  });

  describe("simpleDomains", function() {
    it("Should update the simple domains and highlight changes", function() {
      var row = d3
        .select("body")
        .append("tr")
        .attr("id", "var1");

      row.append("td").text("var1");

      row.append("td").text("int(1)");

      let vars = [{ name: "var1", rng: "int(2)" }];
      let changed = ["var1"];

      // Listener.simpleDomainsHandler({vars: vars, changedNames: changed});

      expect($("#var1").attr("class")).toBe("changed");
    });
  });
});
