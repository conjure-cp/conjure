import * as React from 'react'
import TextWithLabel from './TextWithLabel'
import SelectWithLabel from './SelectWithLabel'
import StageHeader from '../common/StageHeader'
import { Cache, RepMap, newCache } from '../../../../extension/src/utils'

import { Field, FieldProps, FormikProps } from 'formik'
import { MinionConfig, MinionStage } from './MinionStage'
import { SRConfig, SRStage } from './SRStage'
import { ConjureConfig, ConjureStage } from './ConjureStage'
import { ChangeEvent } from 'react'
import Select from 'react-select'

interface Props {
	modelToReps: RepMap
	essenceFiles: string[]
	paramFiles: string[]
	index: number
	caches: Cache[]
	changeHandler: (e: ChangeEvent<HTMLSelectElement>) => void
	fieldSetter: (field: string, value: any) => void
}

interface State {
	showReps: boolean
	currentCache?: Cache
}

export interface CombinedConfig {
	conjureConfig: ConjureConfig
	srConfig: SRConfig
	minionConfig: MinionConfig
}

export interface Values {
	cache: Cache
}

export class ConfigArrayElement extends React.Component<Props & FormikProps<Values> & FieldProps<any>, State> {
	state = {
		showReps: false,
		currentCache: undefined
	}
	render = () => {
		const { index, values } = this.props
		const { name } = this.props.field

		console.log(values)
		return (
			<StageHeader isCollapsed={false} title={`Config ${index + 1}`} id={`config${index + 1}`}>
				<Field name={`${name}.name`} component={TextWithLabel} title={'Save as:'} />
				<Select
					// component={Select}
					id={'cacheSelect'}
					options={[ { value: 'untitled', label: 'untitled' } ].concat(
						this.props.caches.map((x) => {
							return { value: x.name, label: x.name }
						})
					)}
					onChange={(event: any) => {
						// const cacheName = event.target.value
						let chosen = this.props.caches.find((x) => x.name === event.value)!

						if (!chosen) {
							chosen = newCache()
							chosen.essenceFile = this.props.essenceFiles[0]
							chosen.paramFile = this.props.paramFiles[0]
						}

						values.cache = chosen
						this.props.fieldSetter(this.props.field.name, chosen)
						this.setState({
							currentCache: chosen,
							showReps: !values.cache.config.conjureConfig.answers.every((x) => !x)
						})
					}}
				/>

				<Field
					name={`${name}.essenceFile`}
					component={SelectWithLabel}
					title='Model'
					options={this.props.essenceFiles.map((file) => {
						return { value: file, label: file }
					})}
					values={values.cache.essenceFile}
				/>

				<Field
					name={`${name}.paramFile`}
					component={SelectWithLabel}
					title='Parameter'
					options={this.props.paramFiles.map((file) => {
						return { value: file, label: file }
					})}
					values={values.cache.paramFile}
				/>

				<Field
					name={`${name}.config.conjureConfig`}
					component={ConjureStage}
					values={{ config: values.cache.config.conjureConfig }}
					index={index}
					varRepresentations={this.props.modelToReps[values.cache.essenceFile]}
					showReps={this.state.showReps}
					showRepsHandler={() =>
						this.setState((prevState: State) => {
							return { showReps: !prevState.showReps }
						})}
				/>

				<Field
					name={`${name}.config.srConfig`}
					component={SRStage}
					values={{ config: values.cache.config.srConfig }}
					index={index}
				/>

				<Field
					name={`${name}.config.minionConfig`}
					component={MinionStage}
					values={{ config: values.cache.config.minionConfig }}
					index={index}
				/>
			</StageHeader>
		)
	}
}
