import * as React from "react"
import StageHeader from "./StageHeader"
import { Check } from "./Check"
import { MySlider } from "./Slider"

interface Props {
  locked: boolean
  showLabels: boolean
  duration: number
  interval: number
  labelChangeHandler: () => void
  lockChangeHandler: () => void
  durationChangeHandler: (value: number) => void
  intervalChangeHandler: (value: number) => void
  loadDepthChangeHandler: (depth: number) => void
}

interface State {}

class VisualiserSettings extends React.Component<Props, State> {
  constructor(props: any) {
    super(props)
    this.state = {}
  }

  render = () => {
    return (
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
              domain={[1, 10]}
              sliderChangeHandler={this.props.loadDepthChangeHandler}
            />
          </div>
        </div>

        <div className="row">
          <div className="col-2">
            <Check
              title={"lock"}
              checked={this.props.locked}
              onChange={this.props.lockChangeHandler}
            />
          </div>

          <label className="col-2">Animation duration (ms):</label>
          <div className="slider col-2">
            <MySlider
              values={[this.props.duration]}
              domain={[0, 4000]}
              sliderChangeHandler={this.props.durationChangeHandler}
            />
          </div>

          <label className="col-2">Interval between animations (ms):</label>
          <div className="slider col-2">
            <MySlider
              values={[this.props.interval]}
              domain={[0, 4000]}
              sliderChangeHandler={this.props.intervalChangeHandler}
            />
          </div>

          <Check
            title={"Show Labels"}
            checked={this.props.showLabels}
            onChange={this.props.labelChangeHandler}
          />
        </div>
      </StageHeader>
    )
  }
}

export default VisualiserSettings
