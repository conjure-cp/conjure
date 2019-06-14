import * as React from "react";
import * as ReactDOM from "react-dom";
import * as d3 from "d3";
import Node from "../modules/Node";
import TreeVis from "./TreeVis";
import { HierarchyLink, HierarchyPointLink, HierarchyNode } from "d3";
import { any } from "prop-types";
import { HotKeys } from "react-hotkeys";
import * as _ from "lodash";
import { stat } from "fs";

const map = {
  goLeft: ["left", "down"],
  goRight: "right",
  goUp: "up",
  addNode: "a",
  collapse: "c",
  expand: "e"
};

export interface Core {
  nodes: any[];
  solAncestorIds: number[];
}

type MyMap = Record<number, Node>;

interface State {
  id2Node: MyMap;
  selected: number;
}

interface Props {
  core: Core;
}

export class TreeContainer extends React.Component<Props, State> {
  constructor(props: Props) {
    // console.log("constructor!!");

    super(props);
    // this.state = {core: props.core, solNodeIds: [], id2Node: {}}
    this.makeState = this.makeState.bind(this);
    this.addNode = this.addNode.bind(this);
    this.goLeft = this.goLeft.bind(this);
    this.goUp = this.goUp.bind(this);
    this.goRight = this.goRight.bind(this);
    this.collapse = this.collapse.bind(this);
    this.expand = this.expand.bind(this);

    // const data = new Node(0, "rooot node", "blah", -1, 100, true, 2, false);
    // const kid1 = new Node(1, "kid 1", "blah", 0, 100, true, 2, false);
    // const kid2 = new Node(2, "kid 2", "blah", 0, 100, true, 2, false);

    // data.children = [kid1, kid2];

    // let initMap: MyMap = {};
    // initMap[0] = data;
    // initMap[1] = kid1;
    // initMap[2] = kid2;

    // this.state = { id2Node: initMap, selected: 0 };
    // this.state = this.makeState(this.props.core);
  }

  componentDidMount() {}

  makeState(core: Core): State {
    let state: any = {};
    state.solNodeIds = [];
    state.id2Node = {};
    state.selected = 0;

    for (let i = 0; i < core.nodes.length; i++) {
      const newNode = core.nodes[i];
      const parentId = newNode.parentId;

      if (newNode.isSolution) {
        state.solNodeIds.push(newNode.id);
      }

      if (newNode.parentId === -1) {
        state.id2Node[newNode.id] = newNode;
        continue;
      }

      if (!state.id2Node[parentId].children) {
        state.id2Node[parentId].children = [];
      }

      if (newNode.isLeftChild) {
        state.id2Node[parentId].children!.unshift(newNode);
      } else {
        state.id2Node[parentId].children!.push(newNode);
      }

      state.id2Node[newNode.id] = newNode;
    }

    return state;
  }

  addNode() {
    const newNode = new Node(10, "newboy", "new", 2, 100, true, 2, false);

    this.setState((prevState: State) => {
      let newMap = _.extend({}, prevState.id2Node);

      if (!newMap[2].children) {
        newMap[2].children = [];
      }

      newMap[2].children.push(newNode);
      return { id2Node: newMap };
    });
  }

  goLeft() {
    this.setState((prev: State) => {
      const current = prev.id2Node[this.state.selected];
      if (!current.children) {
        return null;
      }
      return { selected: current.children[0].id };
    });
  }

  goRight() {
    this.setState((prev: State) => {
      const current = prev.id2Node[this.state.selected];
      if (!current.children) {
        return null;
      }
      if (current.children.length < 2) {
        return null;
      }
      return { selected: current.children[1].id };
    });
  }

  goUp() {
    this.setState((prev: State) => {
      const current = prev.id2Node[this.state.selected];
      if (current.parentId === -1) {
        return null;
      }
      return { selected: current.parentId };
    });
  }

  collapse() {
    this.setState((prevState: State) => {
      let newMap = _.extend({}, prevState.id2Node);
      newMap[prevState.selected].collapse();
      return { id2Node: newMap };
    });

    // console.log("collasped!");
  }

  expand() {
    this.setState((prevState: State) => {
      let newMap = _.extend({}, prevState.id2Node);
      newMap[prevState.selected].expand();
      return { id2Node: newMap };
    });

    // console.log("expanded!");
  }

  render() {
    const handlers = {
      addNode: this.addNode,
      goLeft: this.goLeft,
      goUp: this.goUp,
      goRight: this.goRight,
      collapse: this.collapse,
      expand: this.expand
    };

    console.log("TrecontainerProps ", this.props);
    const { selected, id2Node } = this.makeState(this.props.core);

    return (
      <HotKeys keyMap={map} handlers={handlers}>
        <div>
          This is the container
          <TreeVis
            rootNode={id2Node[0]}
            selected={selected}
            duration={1000}
            width={500}
            height={500}
          />
        </div>
      </HotKeys>
    );
  }
}
