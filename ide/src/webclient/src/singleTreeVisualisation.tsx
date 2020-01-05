import * as React from 'react'
import * as ReactDOM from 'react-dom'
import StageHeader from './components/common/StageHeader'
import { Cache, RepMap } from '../../extension/src/utils'
import { cloneDeep } from 'lodash'
import Forest from './components/Forest'
import './css/styles.css'
import './css/vis.css'
import { InitResponse } from '../../server/server'
import { ConfigForm } from './components/config/ConfigForm'
import { requestServer } from './modules/TreeHelper'

require('es6-promise').polyfill()
require('isomorphic-fetch')

interface State {
	trees: any
	isCollapsed: boolean
	diff: boolean
	allCaches: Cache[]
	selectedCaches?: (Cache | undefined)[]
	essenceFiles: string[]
	paramFiles: string[]
	modelToReps: RepMap
	nimServerPort: number
	vscodeServerPort: number
	canRender: boolean
	waitingForSolution: boolean
	showError: boolean
	errorObject: any
}

class Root extends React.Component<any, State> {
	// static whyDidYouRender = true;

	constructor(props: any) {
		super(props)
		this.state = {
			waitingForSolution: false,
			showError: false,
			canRender: false,
			trees: undefined,
			isCollapsed: false,
			diff: false,
			allCaches: [],
			paramFiles: [],
			essenceFiles: [],
			modelToReps: {},
			nimServerPort: 5000,
			vscodeServerPort: Number(document.getElementById('port')!.getAttribute('vscodeserverport')),
			errorObject: undefined,
		}
		console.log('hello')
	}

	componentDidMount = async () => {
		const res = await fetch(`http://localhost:${this.state.vscodeServerPort}/test/tree`, {
			method: 'get',
		})
		const json = await res.json()
		this.setState({
			isCollapsed: true,
			trees: json.trees,
			nimServerPort: json.nimServerPort,
			waitingForSolution: false,
		})
	}
	makeRequest = async (url: string, payload: string | null, isNimServer: boolean) => {
		this.setState({ waitingForSolution: true })
		const json = await requestServer(url, payload, isNimServer)
		this.setState({ waitingForSolution: false })

		if (json.stackTrace) {
			this.setState({ showError: true, errorObject: json })
			return null
		}
		return json
	}

	render = () => {
		return !this.state.waitingForSolution ? (
			<div>
				<Forest
					requestHandler={this.makeRequest}
					trees={this.state.trees}
					nimServerPort={this.state.nimServerPort}
				/>
			</div>
		) : (
			<p>TEST WAITING FOR CORE</p>
		)
	}
}

ReactDOM.render(
	<div>
		<Root />
	</div>,
	document.getElementById('root'),
)
