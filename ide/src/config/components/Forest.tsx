import * as React from "react"
import { TreeContainer, Core } from "./TreeContainer"
import { Wrapper } from "./Constants"
import { headers } from "../modules/Helper"
import { isEqual } from "lodash"
import VisualiserSettings from "./VisualiserSettings"
import PlaySettings from "./PlaySettings"
import DiffSettings from "./DiffSettings"

export interface Tree {
  hash: string
  path: string
  info: string
  core: Core
}

interface Props {
  trees: Tree[]
  nimServerPort: number
}

interface State {
  loadDepth: number
  duration: number
  interval: number
  reverse: boolean
  showLabels: boolean
  playing: boolean
  collapseAsExploring: boolean
  diffLocations: number[][]
  currentDiffIndex: number
  diffReady: boolean
  splitScreen: boolean
  locked: boolean
}

class Forest extends React.Component<Props, State> {
  constructor(props: any) {
    super(props)
    this.state = {
      loadDepth: 1,
      duration: 1000,
      interval: 1000,
      reverse: false,
      playing: false,
      collapseAsExploring: false,
      showLabels: true,
      diffLocations: [],
      currentDiffIndex: -1,
      diffReady: false,
      splitScreen: false,
      locked: true
    }
  }

  componentDidUpdate = async (prevProps: Props) => {
    if (isEqual(prevProps.trees, this.props.trees)) {
      return
    }

    if (!this.props.trees) {
      return
    }

    if (this.props.trees.length < 2) {
      this.setState({ splitScreen: true })
      return
    }

    const payload = {
      path1: this.props.trees[0].path,
      path2: this.props.trees[1].path,
      hash1: this.props.trees[0].hash,
      hash2: this.props.trees[1].hash
    }

    this.setState({ diffReady: false, currentDiffIndex: -1 })

    fetch(`http://localhost:${this.props.nimServerPort}/diff`, {
      method: "post",
      headers: headers,
      body: JSON.stringify(payload)
    })
      .then(data => data.json())
      .then(json => this.setState({ diffLocations: json, diffReady: true }))
  }

  render = () => {
    // console.log(this.state.diffLocations)
    // console.log(this.state.currentDiffIndex)

    return (
      <>
        {this.props.trees && (
          <>
            <VisualiserSettings
              locked={this.state.locked}
              showLabels={this.state.showLabels}
              duration={this.state.duration}
              interval={this.state.interval}
              labelChangeHandler={() => {
                this.setState((prevState: State) => {
                  return { showLabels: !prevState.showLabels }
                })
              }}
              loadDepthChangeHandler={(value: number) => {
                this.setState({ loadDepth: value })
              }}
              durationChangeHandler={(value: number) => {
                if (this.state.locked) {
                  this.setState({ duration: value, interval: value })
                } else {
                  this.setState({ duration: value })
                }
              }}
              intervalChangeHandler={(value: number) => {
                if (this.state.locked) {
                  this.setState({ duration: value, interval: value })
                } else {
                  this.setState({ interval: value })
                }
              }}
              lockChangeHandler={() => {
                this.setState((prevState: State) => {
                  return { locked: !prevState.locked }
                })
              }}
            />

            <PlaySettings
              playing={this.state.playing}
              reverse={this.state.reverse}
              collapseAsExploring={this.state.collapseAsExploring}
              collapseAsExploringHandler={() => {
                this.setState((prevState: State) => {
                  return {
                    collapseAsExploring: !prevState.collapseAsExploring
                  }
                })
              }}
              reverseChangeHandler={() => {
                this.setState((prevState: State) => {
                  return { reverse: !prevState.reverse }
                })
              }}
              pPressedHandler={() => {
                this.setState((prevState: State) => {
                  return {
                    playing: !prevState.playing
                  }
                })
              }}
            />

            <DiffSettings
              splitScreen={this.state.splitScreen}
              diffReady={this.state.diffReady}
              diffLocations={this.state.diffLocations}
              currentDiffIndex={this.state.currentDiffIndex}
              trees={this.props.trees}
              splitScreenChangeHandler={() => {
                this.setState((prevState: State) => {
                  return {
                    splitScreen: !prevState.splitScreen
                  }
                })
              }}
              diffChangeHandler={(value: number) => {
                this.setState({ currentDiffIndex: value })
              }}
              nextDiffHandler={() => {
                if (this.state.diffLocations.length === 1) {
                  this.setState({ currentDiffIndex: 0 })
                }

                if (
                  this.state.currentDiffIndex + 1 >
                  this.state.diffLocations.length - 1
                ) {
                  return
                }

                this.setState({
                  currentDiffIndex: this.state.currentDiffIndex + 1
                })
              }}
              prevDiffHandler={() => {
                if (this.state.diffLocations.length === 1) {
                  this.setState({ currentDiffIndex: 0 })
                }

                if (this.state.currentDiffIndex - 1 < 0) {
                  return
                }
                this.setState({
                  currentDiffIndex: this.state.currentDiffIndex - 1
                })
              }}
            />

            <div
              className="forestContainer"
              style={{ height: "85%", display: "flex" }}
            >
              {this.state.splitScreen ? (
                <>
                  {this.props.trees.map((_tree: any, i: number) => (
                    <div
                      key={`${this.props.trees[i].hash}${i}`}
                      className="treeContainer"
                      style={
                        this.props.trees.length == 2
                          ? { width: "50%" }
                          : { width: "100%" }
                      }
                    >
                      <TreeContainer
                        diffParentId={
                          this.state.currentDiffIndex !== -1
                            ? this.state.diffLocations.map(x => x[i])[
                                this.state.currentDiffIndex
                              ]
                            : -1
                        }
                        selected={
                          this.state.currentDiffIndex !== -1
                            ? this.state.diffLocations[
                                this.state.currentDiffIndex
                              ][i]
                            : 0
                        }
                        hash={this.props.trees[i].hash}
                        path={this.props.trees[i].path}
                        nimServerPort={this.props.nimServerPort}
                        info={this.props.trees[i].info}
                        core={this.props.trees[i].core}
                        identifier={`tree${i}`}
                        playing={this.state.playing}
                        loadDepth={this.state.loadDepth}
                        reverse={this.state.reverse}
                        duration={this.state.duration}
                        interval={this.state.interval}
                        finishedPlayingHandler={() =>
                          this.setState({ playing: false })
                        }
                        collapseAsExploring={this.state.collapseAsExploring}
                        showLabels={this.state.showLabels}
                      />
                    </div>
                  ))}
                </>
              ) : (
                <div>This is not a splitscreen</div>
              )}
            </div>
          </>
        )}
      </>
    )
  }
}

export default Forest
