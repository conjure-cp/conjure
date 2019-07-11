import * as React from "react"
import StageHeader from "./StageHeader"
import { TreeContainer } from "./TreeContainer"
import { Check } from "./Check"
import { Wrapper } from "./Constants"
import { MySlider } from "./Slider"
import Play from "./Play"
import { headers } from "../modules/Helper"
import { isEqual } from "lodash"

interface Props {
  trees: any[]
  nimServerPort: number
}

interface State {
  loadDepth: number
  duration: number
  reverse: boolean
  showDecisions: boolean
  playing: boolean
  collapseAsExploring: boolean
  diff: number[][]
  currentDiff: number[]
}

class Forest extends React.Component<Props, State> {
  constructor(props: any) {
    super(props)
    this.state = {
      loadDepth: 1,
      duration: 1000,
      reverse: false,
      playing: false,
      collapseAsExploring: false,
      showDecisions: true,
      diff: [[]],
      currentDiff: [0, 0]
    }
  }

  pPressed = () => {
    this.setState((prevState: State) => {
      return {
        playing: !prevState.playing
      }
    })
  }

  componentDidUpdate = async (prevProps: Props) => {
    if (isEqual(prevProps.trees, this.props.trees)) {
      return
    }

    if (!this.props.trees || this.props.trees.length < 2) {
      return
    }

    const payload = {
      path1: this.props.trees[0].path,
      path2: this.props.trees[1].path
    }

    let response = await fetch(
      `http://localhost:${this.props.nimServerPort}/diff`,
      {
        method: "post",
        headers: headers,
        body: JSON.stringify(payload)
      }
    )

    let json = await response.json()

    this.setState({ diff: json })

    console.log("DIFF", json)
  }

  render = () => {
    console.log("CUrrent DIFF", this.state.currentDiff)

    return (
      <>
        {this.props.trees && (
          <>
            <StageHeader
              title={"Visualiser Settings"}
              id={"visSettings"}
              isCollapsed={false}
            >
              <div className="sliderContainer row">
                <label className="col-3">Lazy loading depth:</label>
                <div className="slider col-3">
                  {/* <MySlider
                    values={[1]}
                    domain={[1, 10]}
                    sliderChangeHandler={(value: number) => {
                      this.setState({ loadDepth: value })
                    }}
                  /> */}
                </div>

                <label className="col-3">Animation duration (ms):</label>
                <div className="slider col-3">
                  <MySlider
                    values={[this.state.duration]}
                    domain={[0, 4000]}
                    sliderChangeHandler={(value: number) => {
                      this.setState({ duration: value })
                    }}
                  />
                </div>

                <Check
                  title={"Show Branching Decisions"}
                  checked={this.state.showDecisions}
                  onChange={() => {
                    this.setState((prevState: State) => {
                      return { showDecisions: !prevState.showDecisions }
                    })
                  }}
                />

                <div className="playSettingsContainer">
                  <StageHeader
                    title="Play Settings"
                    id="playSettings"
                    isCollapsed={false}
                  >
                    <div className=" row">
                      <div className="player mb-3 col-2">
                        <Play
                          clickHandler={this.pPressed}
                          playing={this.state.playing}
                          x={0}
                        />
                      </div>

                      <div className="col-10">
                        <Check
                          title={"Reverse"}
                          checked={this.state.reverse}
                          onChange={() => {
                            this.setState((prevState: State) => {
                              return { reverse: !prevState.reverse }
                            })
                          }}
                        />

                        <Check
                          title={"Collapse when explored"}
                          checked={this.state.collapseAsExploring}
                          onChange={() => {
                            this.setState((prevState: State) => {
                              return {
                                collapseAsExploring: !prevState.collapseAsExploring
                              }
                            })
                          }}
                        />
                      </div>
                    </div>
                  </StageHeader>
                </div>
              </div>
            </StageHeader>

            <StageHeader title={"Diff"} id={"diffSettings"} isCollapsed={false}>
              <MySlider
                values={[0]}
                domain={[0, this.state.diff.length - 1]}
                sliderChangeHandler={(value: number) => {
                  this.setState((prevState: State) => {
                    if (!prevState.diff[value]) {
                      return null
                    }

                    return { currentDiff: prevState.diff[value] }
                  })
                }}
              />
            </StageHeader>

            <Wrapper>
              {this.props.trees.map((_tree: any, i: number) => (
                <TreeContainer
                  selected={this.state.currentDiff[i]}
                  key={this.props.trees[i].path}
                  path={this.props.trees[i].path}
                  nimServerPort={this.props.nimServerPort}
                  info={this.props.trees[i].info}
                  core={this.props.trees[i].core}
                  identifier={`tree${i}`}
                  playing={this.state.playing}
                  loadDepth={this.state.loadDepth}
                  reverse={this.state.reverse}
                  duration={this.state.duration}
                  finishedPlayingHandler={() =>
                    this.setState({ playing: false })
                  }
                  collapseAsExploring={this.state.collapseAsExploring}
                  showDecisions={this.state.showDecisions}
                />
              ))}
            </Wrapper>
          </>
        )}
      </>
    )
  }
}

export default Forest
