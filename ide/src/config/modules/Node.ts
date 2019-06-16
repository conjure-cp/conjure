import * as d3 from "d3";
/**
 * This class represents a d3 hierarchy node
 */

export default class Node {
  public id: number;
  public label: string;
  public name: string = "";
  public prettyLabel: string;
  public parentId: number;
  public children: Node[] | undefined;
  public _children: Node[] | undefined;
  public x: number;
  public y: number;
  public x0: number | null;
  public y0: number | null;
  public depth: number;
  public descCount: number = 0;
  public isLeftChild: boolean;
  public childCount: number;
  public isSolution: boolean;
  private static minRadius = 10;

  constructor(
    id: number,
    label: string,
    prettyLabel: string,
    parentId: number,
    decCount: number,
    isLeftchild: boolean,
    childCount: number,
    isSolution: boolean
  ) {
    this.id = id;
    this.parentId = parentId;
    this.children = undefined;
    this._children = undefined;
    this.x = 0;
    this.y = 0;
    this.x0 = null;
    this.y0 = null;
    this.depth = 0;
    this.descCount = decCount;
    this.label = label;
    this.prettyLabel = prettyLabel;
    this.isLeftChild = isLeftchild;
    this.childCount = childCount;
    this.isSolution = isSolution;
  }

  public static fromNode(old: Node) {
    let n = new Node(
      old.id,
      old.label,
      old.prettyLabel,
      old.parentId,
      old.descCount,
      old.isLeftChild,
      old.childCount,
      old.isSolution
    );
    n.children = old.children;
    n._children = old._children;
    return n;
  }

  public static expandNode(node: Node) {
    let recurse = (insideNode: Node) => {
      for (var i in insideNode._children!) {
        recurse(insideNode._children![Number(i)]);
      }

      Node.showChildren(insideNode);
    };

    recurse(node);
  }
  /**
   * Deeply collapses a node's children
   * @param node
   */

  public static collapseNode(node: Node) {
    let recurse = (insideNode: Node) => {
      for (var i in insideNode.children!) {
        recurse(insideNode.children![Number(i)]);
      }

      Node.hideChildren(insideNode);
    };

    recurse(node);
  }

  /**
   * Unhides the children of a node
   * @param node
   */

  public static showChildren(node: Node) {
    if (node) {
      if (node._children) {
        node.children = node._children;
        node._children = undefined;
      }
    }
  }

  /**
   * Hides the children of a node
   * @param node
   */

  public static hideChildren(node: Node) {
    if (node) {
      if (node.children) {
        node._children = node.children;
        node.children = undefined;
      }
    }
  }

  /**
   * Shows the children of a node if they are hidden.
   * Hides the children of a node if they are showing.
   * @param node
   */

  public static toggleNode(node: Node) {
    if (node._children) {
      Node.showChildren(node);
    } else if (node.children) {
      Node.hideChildren(node);
    }
  }

  /**
   * Returns if the node has children that are either collapse or not yet loaded.
   * @param node
   */

  public static hasMoreChildren(node: Node): boolean {
    let childLength = 0;
    if (node.children) {
      childLength = node.children.length;
    }

    return childLength < node.childCount;
  }

  /**
   * Returns the radius of the node.
   * @param node
   */

  public static calculateRadius(node: Node): number {
    if (node.children && node.children.length === node.childCount) {
      return Node.minRadius;
    }

    // Use logarithmic scale so nodes don't get too big.
    let size = Math.log(node.descCount + Node.minRadius) * 3;

    if (size < Node.minRadius) {
      return Node.minRadius;
    }

    return size;
  }

  /**
   * Returns the height of a nodes descendant count label.
   * @param node
   */
  public static getDescLabelHeight(node: Node) {
    return Node.calculateRadius(node) + 13;
  }
}
