import * as React from "react"
import StageHeader from "./StageHeader"
import { Check } from "./Check"
import { Tree } from "./Forest"
import { MySlider } from "./Slider"
import FlickThru from "./FlickThu"

interface Props {
  trees: Tree[]
  splitScreen: boolean
  diffReady: boolean
  diffLocations: number[][]
  currentDiffIndex: number
  splitScreenChangeHandler: () => void
  diffChangeHandler: (value: number) => void
  prevDiffHandler: () => void
  nextDiffHandler: () => void
}

interface State {}

class DiffSettings extends React.Component<Props, State> {
  constructor(props: any) {
    super(props)
    this.state = {}
  }

  render = () => {
    return (
      <>
        {this.props.trees.length === 2 && (
          <StageHeader title={"Diff"} id={"diffSettings"} isCollapsed={false}>
            <Check
              title={"Split screen"}
              checked={this.props.splitScreen}
              onChange={this.props.splitScreenChangeHandler}
            />

            <>
              {this.props.diffReady ? (
                <>
                  {this.props.diffLocations.length > 0 ? (
                    <>
                      <div>
                        Trees differ in {this.props.diffLocations.length} places
                      </div>

                      {this.props.diffLocations.length > 1 && (
                        <MySlider
                          values={[this.props.currentDiffIndex]}
                          domain={[0, this.props.diffLocations.length - 1]}
                          sliderChangeHandler={this.props.diffChangeHandler}
                        />
                      )}
                      <FlickThru
                        nextHandler={this.props.nextDiffHandler}
                        prevHandler={this.props.prevDiffHandler}
                      />
                    </>
                  ) : (
                    <div>Trees are identical</div>
                  )}
                </>
              ) : (
                <div>Waiting for diff....</div>
              )}
            </>
          </StageHeader>
        )}
      </>
    )
  }
}

export default DiffSettings
