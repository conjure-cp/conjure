import * as React from "react"
import StageHeader from "../common/StageHeader"
import { Check } from "../common/Check"
import { Tree } from "../Forest"
import { MySlider } from "../common/Slider"
import FlickThru from "../common/FlickThu"

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
    const maybeSlider = this.props.diffLocations.length > 1 && (
      <MySlider
        values={[this.props.currentDiffIndex]}
        domain={[0, this.props.diffLocations.length - 1]}
        sliderChangeHandler={this.props.diffChangeHandler}
      />
    )

    const message =
      this.props.diffLocations.length > 0 ? (
        <>
          <div>Trees differ in {this.props.diffLocations.length} places</div>
        </>
      ) : (
        <div>Trees are identical</div>
      )

    const flickThrough = this.props.diffLocations.length > 0 && (
      <FlickThru
        nextHandler={this.props.nextDiffHandler}
        prevHandler={this.props.prevDiffHandler}
      />
    )
    const waiting = <div>Waiting for diff....</div>

    return (
      <>
        {this.props.trees.length === 2 && (
          <StageHeader title={"Diff"} id={"diffSettings"} isCollapsed={false}>
            <div className="row">
              <div className="col-2">
                <Check
                  title={"Split screen"}
                  checked={this.props.splitScreen}
                  onChange={this.props.splitScreenChangeHandler}
                />
              </div>
              {this.props.diffReady && (
                <div className="col-2">
                  <div className="row">{message}</div>
                  <div className="row">
                    {this.props.diffReady && flickThrough}
                  </div>
                </div>
              )}

              {this.props.diffReady && <div className="col">{maybeSlider}</div>}
              {!this.props.diffReady && <div className="col">{waiting}</div>}
            </div>
          </StageHeader>
        )}
      </>
    )
  }
}

export default DiffSettings