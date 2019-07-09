import * as React from "react"
import StageHeader from "./StageHeader"
import { TreeContainer } from "./TreeContainer"
import { Check } from "./Check"
import { Wrapper } from "./Constants"
import { MySlider } from "./Slider"
import Play from "./Play"

interface Props {
  trees: any[]
  nimServerPort: number
}

interface State {
  loadDepth: number
  duration: number
  reverse: boolean
  playing: boolean
}

class Forest extends React.Component<Props, State> {
  constructor(props: any) {
    super(props)
    this.state = {
      loadDepth: 1,
      duration: 500,
      reverse: false,
      playing: false
    }
  }

  pPressed = () => {
    this.setState((prevState: State) => {
      return {
        playing: !prevState.playing
      }
    })
  }

  render = () => {
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
                  <MySlider
                    values={[1]}
                    domain={[1, 5]}
                    sliderChangeHandler={(value: number) => {
                      this.setState({ loadDepth: value })
                    }}
                  />
                </div>

                <label className="col-3">Animation duration (ms):</label>
                <div className="slider col-3">
                  <MySlider
                    values={[500]}
                    domain={[0, 4000]}
                    sliderChangeHandler={(value: number) => {
                      this.setState({ duration: value })
                    }}
                  />
                </div>

                <div className="col">
                  <Check
                    title={"Reverse"}
                    checked={this.state.reverse}
                    onChange={() => {
                      this.setState((prevState: State) => {
                        return { reverse: !prevState.reverse }
                      })
                    }}
                  />
                </div>

                <div className="player col mb-3">
                  <Play
                    clickHandler={this.pPressed}
                    playing={this.state.playing}
                    x={0}
                  />
                </div>
              </div>
            </StageHeader>

            <Wrapper>
              {this.props.trees.map((_tree: any, i: number) => (
                <TreeContainer
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
