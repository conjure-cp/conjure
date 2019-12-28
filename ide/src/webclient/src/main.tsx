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
	canRender: boolean
	waiting: boolean
}

class Root extends React.Component<any, State> {
	// static whyDidYouRender = true;

	constructor(props: any) {
		super(props)
		this.state = {
			waiting: false,
			canRender: false,
			trees: undefined,
			isCollapsed: false,
			diff: false,
			allCaches: [],
			paramFiles: [],
			essenceFiles: [],
			reps: {},
			nimServerPort: 5000,
			vscodeServerPort: Number(document.getElementById('port')!.getAttribute('vscodeserverport'))
		}
		console.log('hello')
	}

	// collapseHandler = () => {
	// 	this.setState((prevState: State) => {
	// 		return { isCollapsed: !prevState.isCollapsed }
	// 	})
	// }

	// diffCheckHandler = (namedCache1: Cache) => {
	// 	// console.log(namedCache1)

	// 	this.setState((prevState: State) => {
	// 		namedCache1.name = ''

	// 		return {
	// 			diff: !prevState.diff,
	// 			selectedCaches: [ namedCache1, cloneDeep(namedCache1) ]
	// 		}
	// 	})
	// }

	initResponseHandler = (data: InitResponse) => {
		this.setState({
			isCollapsed: true,
			trees: data.trees,
			nimServerPort: data.nimServerPort,
			waiting: false
		})
		this.getFiles()
	}

	getFiles = async () => {
		await fetch(`http://localhost:${this.state.vscodeServerPort}/config/files`)
			.then((response) => response.json())
			.then((data) => {
				this.setState({
					paramFiles: data.paramFiles,
					essenceFiles: data.essenceFiles,
					modelToReps: data.modelToReps
				})
				return
			})
			.then(() => {
				fetch(`http://localhost:${this.state.vscodeServerPort}/config/caches`)
					.then((response) => response.json())
					.then((data) => {
						this.setState({
							allCaches: data
						})
					})
			})
			.then(() => this.setState({ canRender: true }))
	}

	componentDidMount = () => {
		this.getFiles()
	}

	render = () => {
		// console.log('prinitg the state')
		// console.log(this.state.allCaches)

		return this.state.canRender ? (
			<div>
				<StageHeader
					title={'Setup'}
					id={'setup'}
					isCollapsed={this.state.isCollapsed}
					// isCollapsed={true}
					// collapseHandler={this.collapseHandler}
				>
					<button
						className='btn btn-danger btn-lg btn-block mb-2'
						onClick={async () => {
							console.log('Should invalidate caches!')
							await fetch(`http://localhost:${this.state.vscodeServerPort}/config/invalidateCaches`)
							await this.getFiles()
							console.log('Done!!')
						}}
					>
						Invalidate Caches
					</button>
					<ConfigForm
						caches={this.state.allCaches}
						waiting={this.state.waiting}
						modelToReps={this.state.modelToReps}
						essenceFiles={this.state.essenceFiles}
						paramFiles={this.state.paramFiles}
						submitHandler={async (values) => {
							this.setState({ waiting: true })
							const res = await fetch(`http://localhost:${this.state.vscodeServerPort}/config/solve`, {
								method: 'post',
								headers: {
									Accept: 'application/json, text/plain, */*',
									'Content-Type': 'application/json'
								},
								body: JSON.stringify(values)
							})
							const json = await res.json()
							this.initResponseHandler(json)
						}}
						// new Promise(() => {
						// 	console.log('Making request')
						// 	// this.setState({ waiting: true })
						// })
						// 	.then(() =>
						// 		fetch(`http://localhost:${this.state.vscodeServerPort}/config/solve`, {
						// 			method: 'post',
						// 			headers: {
						// 				Accept: 'application/json, text/plain, */*',
						// 				'Content-Type': 'application/json'
						// 			},
						// 			body: JSON.stringify(values)
						// 		})
						// 	)
						// 	.then((response) => {
						// 		console.log('HERERERER')
						// 		this.setState({ waiting: false })
						// 		return response
						// 	})
						// 	.then((response) => response.json())
						// 	.then((data) => {
						// 		console.log('From solve', data)
						// 		this.initResponseHandler(data)
						// 		this.setState({ waiting: false })
						// 	})
						// 	.finally(() => console.log('over'))}
					/>
				</StageHeader>
				<Forest trees={this.state.trees} nimServerPort={this.state.nimServerPort} />
			</div>
		) : (
			<p>Waiting for response...</p>
		)
	}
}

ReactDOM.render(
	// <FormikApp email="barrybil@brownmail"/>,
	<div>
		<Root />
		{/* <FormikConjure diff={true}/> */}
	</div>,
	document.getElementById('root')
)
