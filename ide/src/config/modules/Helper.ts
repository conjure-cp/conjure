import { HierarchyPointLink, HierarchyPointNode, Selection } from "d3";
import * as d3 from "d3";
import Node from "../modules/Node";
import { State, MyMap } from "../components/TreeContainer";
import { cloneDeep, last } from "lodash";

export const linkGenerator = d3
  .linkVertical<any, HierarchyPointNode<Node>>()
  .x(d => {
    return d.x;
  })
  .y(d => {
    return d.y;
  });

export const showAllAncestors = (prevState: State, startId: number): MyMap => {
  let newMap = cloneDeep(prevState.id2Node);

  let currentId = newMap[startId].id;

  while (true) {
    Node.showChildren(newMap[currentId]);
    if (currentId === 0) {
      break;
    }
    currentId = newMap[newMap[currentId].parentId].id;
  }

  return newMap;
};

export const getNextSolId = (prevState: State): number => {
  if (!prevState.solveable) {
    return prevState.selected;
  }

  const currentIdInSolNodeIds = prevState.solNodeIds.indexOf(
    prevState.selected
  );

  if (currentIdInSolNodeIds === -1) {
    return prevState.solNodeIds[0];
  }

  if (prevState.solNodeIds.length <= currentIdInSolNodeIds + 1) {
    return prevState.solNodeIds[0];
  }

  return prevState.solNodeIds[currentIdInSolNodeIds + 1];
};

export const getPrevSolId = (prevState: State): number => {
  if (!prevState.solveable) {
    return prevState.selected;
  }

  const currentIdInSolNodeIds = prevState.solNodeIds.indexOf(
    prevState.selected
  );

  if (currentIdInSolNodeIds === -1) {
    return last(prevState.solNodeIds)!;
  }

  if (currentIdInSolNodeIds - 1 < 0) {
    return last(prevState.solNodeIds)!;
  }

  return prevState.solNodeIds[currentIdInSolNodeIds - 1];
};
