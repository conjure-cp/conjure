import { HierarchyPointLink, HierarchyPointNode, Selection } from "d3";
import * as d3 from "d3";
import Node from "../modules/Node";

export const linkGenerator = d3
  .linkVertical<any, HierarchyPointNode<Node>>()
  .x(function(d) {
    return d.x;
  })
  .y(function(d) {
    return d.y;
  });