import * as React from "react";
import * as ReactDOM from "react-dom";
import Node from "../modules/Node";
import TreeVis from "./TreeVis";
import { HotKeys } from "react-hotkeys";
import { cloneDeep } from "lodash";
import * as d3 from "d3";
import { thresholdScott } from "d3";
import { linkGenerator } from "../modules/Helper";

interface Props {
  solNodeIds: number[];
  totalNodes: number;
  failedBranchCount: number;
  minsize: number;
  linScale: any;
}

interface State {}

export default class StatusBar extends React.Component<Props, State> {
  // static whyDidYouRender = true;

  constructor(props: Props) {
    super(props);
    // this.goLeft = this.goLeft.bind(this);
  }

  render() {
    const height = this.props.minsize * 6;
    const width = this.props.minsize * 6;

    const layout = d3.tree<Node>().nodeSize([15, 15]);

    let rootNode = {
      isSolution: true,
      children: [{}, {}, {}]
    };

    const hierarchy = d3.hierarchy<any>(rootNode);

    let n = layout(hierarchy);
    let links = n.links();

    let path = linkGenerator(links[0]);

    if (!path) {
      path = "";
    }

    console.log(
      "RADIUS",
      Node.getRadius(n, this.props.linScale, this.props.minsize)
    );

    return (
      <div className="row">
        <div className="col">
          <label>
            <svg width={width} height={height}>
              <title>Failed Branches</title>
              <g transform={`translate(${30},${11}), scale(1)`}>
                <path className="link red" d={path}></path>
              </g>
            </svg>
            × {this.props.failedBranchCount}
          </label>
        </div>
        <div className="col">
          <label>
            <svg width={width} height={height}>
              <title>Successful Branches</title>
              <g transform={`translate(${30},${11}), scale(1)`}>
                <path className="link " d={path}></path>
              </g>
            </svg>
            × {this.props.totalNodes - 1 - this.props.failedBranchCount}
          </label>
        </div>
        <div className="col">
          <label>
            <svg width={width} height={height}>
              <title>Total number of nodes in the tree</title>
              <g>
                <g className="node">
                  <circle
                    r={this.props.minsize}
                    cx={width / 2}
                    cy={height / 2 - 2}
                  ></circle>
                </g>
              </g>
            </svg>
            × {this.props.totalNodes}
          </label>
        </div>
        <div className="col">
          <label>
            <svg width={width} height={height}>
              <title>Number of Solutions found</title>
              <g>
                <g className="node">
                  <circle
                    className="solution"
                    r={Node.getRadius(
                      n,
                      this.props.linScale,
                      this.props.minsize
                    )}
                    cx={width / 2}
                    cy={height / 2 - 2}
                  ></circle>
                </g>
              </g>
            </svg>
            × {this.props.solNodeIds.length}
          </label>
        </div>
      </div>
    );
  }
}
