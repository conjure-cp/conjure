import * as React from 'react'
import StageHeader from '../common/StageHeader'
import { Check } from '../common/Check'
import { MySlider } from '../common/Slider'

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
			<StageHeader title={'Visualiser Settings'} id={'visSettings'} isCollapsed={true}>
				<div className='row'>
					<div className='col-3'>
						<Check
							title={'Show Labels'}
							checked={this.props.showLabels}
							onChange={this.props.labelChangeHandler}
						/>
						<Check
							title={'Sync animation to delay'}
							checked={this.props.locked}
							onChange={this.props.lockChangeHandler}
						/>
					</div>

					<div className='slider col-3'>
						<label>Lazy loading depth:</label>
						<MySlider
							values={[ 1 ]}
							domain={[ 1, 10 ]}
							sliderChangeHandler={this.props.loadDepthChangeHandler}
						/>
					</div>

					<div className='slider col-3'>
						<label>Animation duration (ms):</label>
						<MySlider
							values={[ this.props.duration ]}
							domain={[ 0, 4000 ]}
							sliderChangeHandler={this.props.durationChangeHandler}
						/>
					</div>

					<div className='slider col-3'>
						<label>Interval between animations (ms):</label>
						<MySlider
							values={[ this.props.interval ]}
							domain={[ 0, 4000 ]}
							sliderChangeHandler={this.props.intervalChangeHandler}
						/>
					</div>
				</div>
			</StageHeader>
		)
	}
}

export default VisualiserSettings
