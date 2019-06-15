import * as React from "react";
import * as ReactDOM from "react-dom";
import Node from "../modules/Node";
import TreeVis from "./TreeVis";
import { HotKeys } from "react-hotkeys";
import { cloneDeep } from "lodash";
import * as d3 from "d3";

export type MyMap = Record<number, Node>;

export interface Core {
  nodes: Node[];
  solAncestorIds: number[];
  id: string;
}

interface Props {
  core: Core;
}

interface State {
  id2Node: MyMap;
  solveable: boolean;
  selected: number;
  linScale: any;
  minsize: number;
}

function makeState(core: Core): State {
  const minsize = 7;
  const solveable = core.nodes.find(n => n.isSolution) !== undefined;
  const linScale = d3
    .scaleLinear()
    .domain([0, core.nodes[0].descCount]) // upper domain is Way more just to be safe
    .range([minsize, 30]);

  let id2Node: MyMap = {};

  for (let i = 0; i < core.nodes.length; i++) {
    const newNode = core.nodes[i];
    const parentId = newNode.parentId;
    if (newNode.parentId === -1) {
      id2Node[newNode.id] = newNode;
      continue;
    }
    if (!id2Node[parentId].children) {
      id2Node[parentId].children = [];
    }
    if (newNode.isLeftChild) {
      id2Node[parentId].children!.unshift(newNode);
    } else {
      id2Node[parentId].children!.push(newNode);
    }
    id2Node[newNode.id] = newNode;
  }

  let state: State = {
    id2Node: id2Node,
    minsize: minsize,
    solveable: solveable,
    linScale: linScale,
    selected: 0
  };

  return state;
}

export class TreeContainer extends React.Component<Props, State> {
  static whyDidYouRender = true;

  handlers: any;

  constructor(props: Props) {
    super(props);

    this.addNode = this.addNode.bind(this);
    this.goLeft = this.goLeft.bind(this);
    this.goUp = this.goUp.bind(this);
    this.goRight = this.goRight.bind(this);
    this.collapse = this.collapse.bind(this);
    this.expand = this.expand.bind(this);

    this.state = makeState(this.props.core);

    this.handlers = {
      addNode: this.addNode,
      goLeft: this.goLeft,
      goUp: this.goUp,
      goRight: this.goRight,
      collapse: this.collapse,
      expand: this.expand
    };
  }

  addNode() {
    const newNode = new Node(10, "newboy", "new", 2, 100, true, 2, false);

    this.setState((prevState: State) => {
      let newMap = cloneDeep(prevState.id2Node);

      if (!newMap[2].children) {
        newMap[2].children = [];
      }

      newMap[2].children.push(newNode);
      return { id2Node: newMap };
    });
  }

  goLeft() {
    // console.log("moving");
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
      let newMap = cloneDeep(prevState.id2Node);
      Node.collapseNode(newMap[prevState.selected]);
      return { id2Node: newMap };
    });
  }

  expand() {
    this.setState((prevState: State) => {
      let newMap = cloneDeep(prevState.id2Node);
      Node.expandNode(newMap[prevState.selected]);
      return { id2Node: newMap };
    });

    // console.log("expanded!");
  }

  componentDidUpdate(prevProps: Props) {
    // Typical usage (don't forget to compare props):
    if (this.props.core.id !== prevProps.core.id) {
      this.setState(makeState(this.props.core));
    }
  }

  render() {
    return (
      <HotKeys keyMap={map} handlers={this.handlers}>
        <div>
          This is the container
          <TreeVis
            id={this.props.core.id}
            rootNode={this.state.id2Node[0]}
            selected={this.state.selected}
            solAncestorIds={this.props.core.solAncestorIds}
            solveable={this.state.solveable}
            linScale={this.state.linScale}
            minsize={this.state.minsize}
            duration={500}
            width={500}
            height={500}
          />
        </div>
      </HotKeys>
    );
  }
}

const map = {
  goLeft: ["left", "down"],
  goRight: "right",
  goUp: "up",
  addNode: "a",
  collapse: "c",
  expand: "e"
};
