import * as React from 'react'
import { Check } from '../common/Check'
import { validationSchema } from './Validation'
import { Cache, RepMap, newCache } from '../../../../extension/src/utils'

import { Form, Field, Formik, FormikProps, validateYupSchema } from 'formik'

import { cloneDeep } from 'lodash'
import { ConfigArrayElement } from './ConfigArrayElement'

var Loader = require('react-loader')

interface Props {
	caches: Cache[]
	waiting: boolean
	modelToReps: RepMap
	essenceFiles: string[]
	paramFiles: string[]
	submitHandler: (values: Values, diff: boolean) => void
}

interface State {
	diff: boolean
	// currentCache?: Cache
}

interface Values {
	caches: Cache[]
}

export class ConfigForm extends React.Component<Props, State> {
	state = {
		diff: false,
		currentCache: undefined,
	}

	validateProps = () => {
		if (this.props.essenceFiles.length === 0) {
			return 'No essence files detected!'
		}
		if (this.props.paramFiles.length === 0) {
			return 'No param files detected!'
		}

		for (const cache of this.props.caches) {
			if (!this.props.essenceFiles.includes(cache.essenceFile)) {
				return 'There exists a cache referencing an essence file that does not exist.'
			}
			if (!this.props.paramFiles.includes(cache.paramFile)) {
				return 'There exists a cache referencing an param file that does not exist.'
			}
		}

		for (const essenceFile of this.props.essenceFiles) {
			if (!this.props.modelToReps[essenceFile]) {
				return 'There exists an essenceFile without a corresponding "reps" field.'
			}
		}

		return null
	}

	render = () => {
		const initialCache = {
			...newCache(),
			essenceFile: this.props.essenceFiles[0],
			paramFile: this.props.paramFiles[0],
		}

		const initialValues = { caches: [ initialCache, cloneDeep(initialCache) ] }
		const propsValidationError = this.validateProps()

		const form = (
			<Formik
				initialValues={initialValues}
				onSubmit={(values) => {
					this.props.submitHandler(values, this.state.diff)
				}}
				validationSchema={validationSchema}
				isInitialValid={() => validateYupSchema(initialValues, validationSchema, true) !== undefined}
				enableReinitialize={true}
				render={(renderProps: FormikProps<Values>) => {
					const values = renderProps.values
					const submitButtonMessage = renderProps.isValid ? 'Solve' : 'Fix the errors first!'
					const colourClass = renderProps.isValid ? 'primary' : 'warning'
					const arrayIndexes = this.state.diff ? [ 0, 1 ] : [ 0 ]

					const array = arrayIndexes.map((index) => {
						let currentCache: Cache
						if (this.state.currentCache) {
							currentCache = this.state.currentCache!
						} else {
							currentCache = values.caches[index]
						}
						return (
							<div className='col' key={index}>
								<Field
									name={`caches[${index}]`}
									component={ConfigArrayElement}
									modelToReps={this.props.modelToReps}
									essenceFiles={this.props.essenceFiles}
									paramFiles={this.props.paramFiles}
									index={index}
									caches={this.props.caches}
									values={{ cache: currentCache }}
									fieldSetter={renderProps.setFieldValue}
								/>
							</div>
						)
					})

					return (
						<Form>
							<Check
								title={'Compare trees'}
								checked={this.state.diff}
								onChange={() => {
									this.setState((prevState: State) => {
										return { diff: !prevState.diff }
									})
								}}
							/>

							<div className='row'>{array}</div>

							<Loader loaded={!this.props.waiting}>
								<button type='submit' className={`btn btn-${colourClass} btn-lg btn-block`}>
									{submitButtonMessage}
								</button>
							</Loader>
						</Form>
					)
				}}
			/>
		)

		return propsValidationError === null ? form : propsValidationError
	}
}
