import * as React from 'react'
import * as ReactDOM from 'react-dom'
import StageHeader from './components/common/StageHeader'
import { Cache, RepMap } from '../../extension/src/utils'
import { cloneDeep } from 'lodash'
import Forest from './components/Forest'
import './css/styles.css'
import './css/vis.css'
import { InitResponse, Error } from '../../server/server'
import { ConfigForm } from './components/config/ConfigForm'
import { requestServer } from './modules/TreeHelper'

var Loader = require('react-loader')

require('es6-promise').polyfill()
require('isomorphic-fetch')

if (process.env.NODE_ENV !== 'production') {
	const whyDidYouRender = require('@welldone-software/why-did-you-render/dist/no-classes-transpile/umd/whyDidYouRender.min.js')
	whyDidYouRender(React)
}

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
	filesReceived: boolean
	waitingForSolution: boolean
	invalidateWaiting: boolean
	showError: boolean
	errorObject: Error
}

class Root extends React.Component<any, State> {
	// static whyDidYouRender = true;

	constructor(props: any) {
		super(props)
		this.state = {
			waitingForSolution: false,
			invalidateWaiting: false,
			filesReceived: false,
			trees: undefined,
			isCollapsed: false,
			diff: false,
			allCaches: [],
			paramFiles: [],
			essenceFiles: [],
			modelToReps: {},
			nimServerPort: 5000,
			vscodeServerPort: Number(document.getElementById('port')!.getAttribute('vscodeserverport')),
			showError: false,
			errorObject: { message: 'No error', stackTrace: 'No stack trace', url: 'nourl', reqInfo: 'noInfo' },
		}
		console.log('hello')
	}

	initResponseHandler = async (data: InitResponse) => {
		console.log('fromvscode', data)
		this.setState({
			isCollapsed: true,
			trees: data.trees,
			nimServerPort: data.nimServerPort,
			waitingForSolution: false,
		})
		await this.getFiles()
	}

	getFiles = async () => {
		const filesRes = await this.makeRequest(
			`http://localhost:${this.state.vscodeServerPort}/config/files`,
			null,
			false,
		)
		console.log(filesRes)
		this.setState({
			paramFiles: filesRes.paramFiles,
			essenceFiles: filesRes.essenceFiles,
			modelToReps: filesRes.modelToReps,
		})
		const cachesRes = await this.makeRequest(
			`http://localhost:${this.state.vscodeServerPort}/config/caches`,
			null,
			false,
		)
		this.setState({ allCaches: cachesRes, filesReceived: true })
	}

	componentDidMount = () => {
		this.getFiles()
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
		// console.log('files', this.state.essenceFiles)
		return !this.state.showError ? (
			<div>
				<StageHeader
					title={'Setup'}
					id={'setup'}
					isCollapsed={this.state.isCollapsed}
					// isCollapsed={true}
					// collapseHandler={this.collapseHandler}
				>
					<Loader loaded={!this.state.invalidateWaiting}>
						<button
							className='btn btn-danger btn-lg btn-block mb-2'
							onClick={async () => {
								this.setState({ invalidateWaiting: true })
								await this.makeRequest(
									`http://localhost:${this.state.vscodeServerPort}/config/invalidateCaches`,
									null,
									false,
								)
								await this.getFiles()
								this.setState({ invalidateWaiting: false })
							}}
						>
							Invalidate Caches
						</button>
					</Loader>

					<ConfigForm
						caches={this.state.allCaches}
						waiting={this.state.waitingForSolution}
						modelToReps={this.state.modelToReps}
						essenceFiles={this.state.essenceFiles}
						paramFiles={this.state.paramFiles}
						submitHandler={async (values, isDiffing) => {
							const toSubmit = isDiffing ? values.caches : values.caches.slice(0, 1)

							const res = await this.makeRequest(
								`http://localhost:${this.state.vscodeServerPort}/config/solve`,
								JSON.stringify(toSubmit),
								false,
							)
							this.initResponseHandler(res)
						}}
					/>
				</StageHeader>
				<Forest
					requestHandler={this.makeRequest}
					trees={this.state.trees}
					nimServerPort={this.state.nimServerPort}
				/>
			</div>
		) : (
			<div>
				<p>ERROR</p>
				<p>{this.state.errorObject.message}</p>
				<p>{this.state.errorObject.stackTrace}</p>
				<p>{this.state.errorObject.url}</p>
				<p>{JSON.stringify(this.state.errorObject.reqInfo)}</p>
			</div>
		)
	}
}

ReactDOM.render(
	// <FormikApp email="barrybil@brownmail"/>,
	<div>
		<Root />
		{/* <FormikConjure diff={true}/> */}
	</div>,
	document.getElementById('root'),
)
