import * as React from "react";
import * as ReactDOM from "react-dom";
import Node from "../modules/Node";
import TreeVis from "./TreeVis";
import StatsBar from "./StatsBar";
import { HotKeys } from "react-hotkeys";
import { cloneDeep } from "lodash";
import * as d3 from "d3";
import StageHeader from "./StageHeader";
import { Domains } from "./Domains";
import { thresholdScott, HierarchyCircularNode, HierarchyPointNode } from "d3";

interface FromServerNode {
  id: number;
  parentId: number;
  label: string;
  prettyLabel: string;
  childCount: number;
  isSolution: boolean;
  isLeftChild: boolean;
  descCount: number;
}

export type MyMap = Record<number, Node>;

export interface Core {
  nodes: FromServerNode[];
  solAncestorIds: number[];
  id: string;
}

interface Props {
  identifier: string;
  core: Core;
}

interface State {
  id2Node: MyMap;
  solveable: boolean;
  selected: number;
  linScale: any;
  minsize: number;
  shouldGetKids: boolean;
  playing: boolean;
  solNodeIds: number[];
}

function makeState(core: Core): State {
  const minsize = 7;
  const solveable = core.nodes.find(n => n.isSolution) !== undefined;
  const linScale = d3
    .scaleLinear()
    .domain([0, core.nodes[0].descCount]) // upper domain is Way more just to be safe
    .range([minsize, 30]);

  let id2Node: MyMap = {};

  let solNodeIds = [];

  for (let i = 0; i < core.nodes.length; i++) {
    const element = core.nodes[i];

    if (element.isSolution) {
      solNodeIds.push(element.id);
    }

    const newNode = new Node(
      element.id,
      element.label,
      element.prettyLabel,
      element.parentId,
      element.descCount,
      element.isLeftChild,
      element.childCount,
      element.isSolution
    );

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
    selected: 0,
    shouldGetKids: false,
    playing: false,
    solNodeIds: solNodeIds
  };

  return state;
}

export class TreeContainer extends React.Component<Props, State> {
  // static whyDidYouRender = true;

  handlers: any;

  constructor(props: Props) {
    super(props);

    this.goLeft = this.goLeft.bind(this);
    this.goUp = this.goUp.bind(this);
    this.goRight = this.goRight.bind(this);
    this.collapse = this.collapse.bind(this);
    this.expand = this.expand.bind(this);
    this.nodeClickHandler = this.nodeClickHandler.bind(this);
    this.play = this.play.bind(this);
    this.pPressed = this.pPressed.bind(this);
    this.goToRoot = this.goToRoot.bind(this);

    this.state = makeState(this.props.core);

    this.handlers = {
      goLeft: this.goLeft,
      goUp: this.goUp,
      goRight: this.goRight,
      collapse: this.collapse,
      expand: this.expand,
      pPressed: this.pPressed,
      goToRoot: this.goToRoot
    };
  }

  nodeClickHandler(d: Node) {
    this.setState({ selected: d.id });
  }

  goLeft() {
    this.setState((prevState: State) => {
      let newMap = cloneDeep(prevState.id2Node);
      const current = newMap[prevState.selected];
      const next = prevState.selected + 1;

      if (current._children) {
        Node.showChildren(current);
        return { id2Node: newMap, selected: prevState.selected };
      }

      if (newMap[next]) {
        return { id2Node: newMap, selected: next };
      }

      if (!current.children && current.childCount > 0) {
        fetch(`http://localhost:5000/loadNodes/${prevState.selected}`)
          .then(data => data.json())
          .then(kids => {
            let newMap = cloneDeep(prevState.id2Node);
            newMap[prevState.selected].children = kids;
            newMap[prevState.selected].children!.map((child: Node) => {
              newMap[child.id] = child;
              this.setState({ id2Node: newMap, selected: prevState.selected });
            });
          });
      }

      return null;
    });
  }

  goRight() {
    this.setState((prev: State) => {
      const current = prev.id2Node[prev.selected];
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
      const current = prev.id2Node[prev.selected];
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

  pPressed() {
    this.setState((prevState: State) => {
      return {
        playing: !prevState.playing
      };
    });
  }

  goToRoot() {
    this.setState({ selected: 0 });
  }

  async play() {
    const interval = 400;
    while (this.state.playing) {
      if (this.state.selected === this.state.id2Node[0].descCount) {
        break;
      }
      this.goLeft();
      await this.sleep(interval);
    }
    this.setState({ playing: false });
  }

  sleep(ms: number) {
    return new Promise(resolve => setTimeout(resolve, ms));
  }

  componentDidUpdate(prevProps: Props, prevState: State) {
    // Typical usage (don't forget to compare props):
    if (this.props.core.id !== prevProps.core.id) {
      this.setState(makeState(this.props.core));
    }

    if (this.state.playing !== prevState.playing) {
      this.play();
    }
  }

  render() {
    let failedBranchCount =
      this.state.id2Node[0].descCount -
      (this.state.solveable ? this.props.core.solAncestorIds.length : 0);

    return (
      <HotKeys keyMap={map} handlers={this.handlers}>
        <div>
          <StatsBar
            minsize={this.state.minsize}
            solNodeIds={this.state.solNodeIds}
            totalNodes={this.state.id2Node[0].descCount + 1}
            failedBranchCount={failedBranchCount}
            linScale={this.state.linScale}
          />

          <TreeVis
            id={this.props.core.id}
            identifier={this.props.identifier}
            rootNode={this.state.id2Node[0]}
            selected={this.state.selected}
            solAncestorIds={this.props.core.solAncestorIds}
            solveable={this.state.solveable}
            linScale={this.state.linScale}
            minsize={this.state.minsize}
            nodeClickHandler={this.nodeClickHandler}
            duration={500}
            width={600}
            height={400}
          />

          <Domains id={this.props.core.id} selected={this.state.selected} />
        </div>
      </HotKeys>
    );
  }
}

const map = {
  goLeft: ["left", "s", "a", "down"],
  goRight: ["right", "d"],
  goUp: ["up", "w"],
  collapse: "c",
  expand: "e",
  pPressed: "p",
  goToRoot: "r"
};
