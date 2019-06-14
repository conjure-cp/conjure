import * as React from "react";
import * as ReactDOM from "react-dom";
import * as d3 from "d3";
import Node from "../modules/Node";

interface Props {
  src: Node;
  dest: Node;
}

interface State {}

export default class TreeNode extends React.Component<Props, State> {
  constructor(props: Props) {
    super(props);
    this.state = {};
  }

  render() {
    return (
      <g className={"link"}>
        {/* <circle cx={this.props.x} cy={this.props.y} r={this.props.r} fill="red"></circle> */}
      </g>
    );
  }
}
