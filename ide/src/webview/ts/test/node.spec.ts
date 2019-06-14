import Node from "../src/modules/Node";

describe("Test Node Class ", function() {
  describe("The constructor ", function() {
    it("should have attributes correctly set", function() {
      let root = new Node(0, "simple", "pretty", null, 2, true, 2, false);
      expect(root.parent).toBeNull();
      expect(root.children).toBeNull();
      expect(root._children).toBeNull();
      expect(root.label).toBe("simple");
      expect(root.prettyLabel).toBe("pretty");
      expect(root.descCount).toBe(2);
      expect(root.isLeftChild).toBe(true);
      expect(root.isSolution).toBe(false);
    });
  });

  describe("Expand", function() {
    it("should expand the children of a node with children", function() {
      let rootNode = new Node(0, "simple", "pretty", null, 2, true, 2, false);
      let child = new Node(1, "s", "p", rootNode, 0, true, 0, false);
      rootNode._children = [child];
      Node.expandNode(rootNode);

      expect(rootNode._children).toBeNull();
      expect(rootNode.children.length).toBe(1);
      expect(rootNode.children[0]).toBe(child);
    });
  });

  describe("Collapse", function() {
    it("should collapse the children of a node", function() {
      let rootNode = new Node(0, "simple", "pretty", null, 2, true, 2, false);
      let child = new Node(1, "s", "p", rootNode, 0, true, 0, false);
      rootNode.children = [child];
      Node.collapseNode(rootNode);

      expect(rootNode.children).toBeNull();
      expect(rootNode._children.length).toBe(1);
      expect(rootNode._children[0]).toBe(child);
    });
  });

  describe("Toggle", function() {
    it("should toggle the children of a node", function() {
      let rootNode = new Node(0, "simple", "pretty", null, 2, true, 2, false);
      let child = new Node(1, "s", "p", rootNode, 0, true, 0, false);
      rootNode.children = [child];
      Node.toggleNode(rootNode);

      expect(rootNode.children).toBeNull();
      expect(rootNode._children.length).toBe(1);
      expect(rootNode._children[0]).toBe(child);

      Node.toggleNode(rootNode);
      expect(rootNode._children).toBeNull();
      expect(rootNode.children.length).toBe(1);
      expect(rootNode.children[0]).toBe(child);
    });
  });

  describe("Has more children", function() {
    it("should return if the node has children yet to be loaded", function() {
      let hasMore = new Node(0, "simple", "pretty", null, 2, true, 2, false);
      expect(Node.hasMoreChildren(hasMore)).toBe(true);

      let leaf = new Node(1, "simple", "pretty", hasMore, 0, true, 0, false);
      expect(Node.hasMoreChildren(leaf)).toBe(false);
    });
    it("should return if the node has children that are loaded", function() {
      let parent = new Node(0, "simple", "pretty", null, 2, true, 2, false);
      let child1 = new Node(1, "s", "p", parent, 0, true, 0, false);
      parent.children = [child1];

      expect(Node.hasMoreChildren(parent)).toBe(true);

      let child2 = new Node(2, "s", "p", parent, 0, true, 0, false);
      parent.children.push(child2);

      expect(Node.hasMoreChildren(parent)).toBe(false);
    });
  });

  describe("Calculate radius", function() {
    it("The radius should be no smaller than 10", function() {
      let node = new Node(0, "simple", "pretty", null, 0, true, 2, false);
      expect(Node.calculateRadius(node)).toBe(10);
    });
    it("The radius should be smaller than 50", function() {
      let node = new Node(
        0,
        "simple",
        "pretty",
        null,
        10000000,
        true,
        2,
        false
      );
      expect(Node.calculateRadius(node)).toBeLessThan(50);
    });
  });

  describe("Calculate Descendant count label y poistion", function() {
    it("The label height should be 23", function() {
      let node = new Node(0, "simple", "pretty", null, 0, true, 2, false);
      expect(Node.getDescLabelHeight(node)).toBe(23);
    });
  });
});
