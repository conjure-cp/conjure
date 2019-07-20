import { HierarchyPointNode } from "d3"
import * as ReactDOM from "react-dom"
/**
 * This class represents a d3 hierarchy node
 */

export enum WhichTree {
  Left,
  Right,
  Both
}

export default class Node {
  public id: number
  public label: string
  public name: string = ""
  public prettyLabel: string
  public parentId: number
  public children?: Node[]
  public _children?: Node[]
  public x0: number | null
  public y0: number | null
  public depth: number
  public descCount: number = 0
  public isLeftChild: boolean
  public childCount: number
  public isSolution: boolean
  public treeId: WhichTree

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
    this.treeId = WhichTree.Both
    this.id = id
    this.x0 = null
    this.y0 = null
    this.parentId = parentId
    this.children = undefined
    this._children = undefined
    this.depth = 0
    this.descCount = decCount
    this.label = label
    this.prettyLabel = prettyLabel
    this.isLeftChild = isLeftchild
    this.childCount = childCount
    this.isSolution = isSolution
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
    )
    n.children = old.children
    n._children = old._children
    return n
  }

  public static getRadius(
    d: HierarchyPointNode<Node>,
    linScale: any,
    minsize: number,
    diffParentId: number
  ): number {
    const elem = document.getElementById("root")
    const solNodeStrokeWidth = getComputedStyle(elem!).getPropertyValue(
      "--sol-stroke-width"
    )
    const normalNodeStrokeWidth = getComputedStyle(
      document.getElementById("root")!
    ).getPropertyValue("--circle-stroke-width")

    if (d.data.isSolution || d.data.id === diffParentId) {
      return (
        minsize + Number(normalNodeStrokeWidth) - Number(solNodeStrokeWidth)
      )
    }

    return (d.children ? d.children.length : 0) < d.data.childCount
      ? linScale(d.data.descCount)
      : minsize
  }

  public static expandNode(node: Node) {
    let recurse = (insideNode: Node) => {
      for (var i in insideNode._children!) {
        recurse(insideNode._children![Number(i)])
      }

      Node.showChildren(insideNode)
    }

    recurse(node)
  }
  /**
   * Deeply collapses a node's children
   * @param node
   */

  public static collapseNode(node: Node) {
    let recurse = (insideNode: Node) => {
      for (var i in insideNode.children!) {
        recurse(insideNode.children![Number(i)])
      }

      Node.hideChildren(insideNode)
    }

    recurse(node)
  }

  /**
   * Unhides the children of a node
   * @param node
   */

  public static showChildren(node: Node) {
    if (node) {
      if (node._children) {
        node.children = node._children
        node._children = undefined
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
        node._children = node.children
        node.children = undefined
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
      Node.showChildren(node)
    } else if (node.children) {
      Node.hideChildren(node)
    }
  }
}
