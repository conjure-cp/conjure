import * as React from "react";
import * as ReactDOM from "react-dom";
import * as d3 from "d3";
import Node from "../modules/Node";

interface Props {
  x: number;
  y: number;
  r: number;
}

interface State {}

export default class TreeNode extends React.Component<Props, State> {
  constructor(props: Props) {
    super(props);
    this.state = {};
  }

  render() {
    return (
      <circle
        cx={this.props.x}
        cy={this.props.y}
        r={this.props.r}
        fill="red"
      ></circle>
    );
  }
}
