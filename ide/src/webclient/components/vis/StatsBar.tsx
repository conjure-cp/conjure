import * as React from "react"
import Node from "../../modules/Node"
import FlickThru from "../common/FlickThu"
import * as d3 from "d3"
import { linkGenerator, getNextSolId } from "../../modules/TreeHelper"

interface Props {
  solNodeIds: number[]
  totalNodes: number
  failedBranchCount: number
  minsize: number
  linScale: any
  info: string
  nextSolHandler: () => void
  prevSolHandler: () => void
  nextFailedHandler: () => void
  prevFailedHandler: () => void
  nextNodeHandler: () => void
  prevNodeHandler: () => void
  nextSolBranchHandler: () => void
  prevSolBranchHandler: () => void
}

interface State {}

export default class StatusBar extends React.Component<Props, State> {
  // static whyDidYouRender = true;

  constructor(props: Props) {
    super(props)
    console.log(props.info)
    // this.goLeft = this.goLeft.bind(this);
  }

  render() {
    const height = this.props.minsize * 6
    const width = this.props.minsize * 6

    const layout = d3.tree<Node>().nodeSize([15, 15])

    let rootNode = {
      isSolution: true,
      children: [{}, {}, {}]
    }

    const hierarchy = d3.hierarchy<any>(rootNode)

    let n = layout(hierarchy)
    let links = n.links()

    let path = linkGenerator(links[0])

    if (!path) {
      path = ""
    }

    // console.log(
    // "RADIUS",
    // Node.getRadius(n, this.props.linScale, this.props.minsize)
    // );

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
          <FlickThru
            nextHandler={this.props.nextFailedHandler}
            prevHandler={this.props.prevFailedHandler}
          />
        </div>

        <div className="col">
          <label>
            <svg width={width} height={height}>
              <title>Successful Branches</title>
              <g transform={`translate(${30},${11}), scale(1)`}>
                <path className="link " d={path}></path>
              </g>
            </svg>
            ×{" "}
            {this.props.totalNodes - 1 - this.props.failedBranchCount >= 0
              ? this.props.totalNodes - 1 - this.props.failedBranchCount
              : 0}
          </label>
          <FlickThru
            nextHandler={this.props.nextSolBranchHandler}
            prevHandler={this.props.prevSolBranchHandler}
          />
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
          <FlickThru
            nextHandler={this.props.nextNodeHandler}
            prevHandler={this.props.prevNodeHandler}
          />
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
                      this.props.minsize,
                      -1
                    )}
                    cx={width / 2}
                    cy={height / 2 - 2}
                  ></circle>
                </g>
              </g>
            </svg>
            × {this.props.solNodeIds.length}
          </label>
          {this.props.solNodeIds.length > 0 && (
            <FlickThru
              nextHandler={this.props.nextSolHandler}
              prevHandler={this.props.prevSolHandler}
            />
          )}
        </div>

        <div className="col-1">
          <svg width={width} height={height} transform="translate(0, 7)">
            <title>{this.props.info}</title>
            <path d="M12,2C6.477,2,2,6.477,2,12s4.477,10,10,10s10-4.477,10-10S17.523,2,12,2z M13,17h-2v-6h2V17z M13,9h-2V7h2V9z" />
          </svg>
        </div>
      </div>
    )
  }
}
