import * as React from 'react'
import TextWithLabel from './TextWithLabel'
import SelectWithLabel from './SelectWithLabel'
import StageHeader from '../common/StageHeader'
import Checkbox from './Checkbox'

import { Form, Field, FieldArray, FieldProps, Formik, FormikProps } from 'formik'

interface Props {
	index: number
}

export interface SRConfig {
	optimisation: string
	symmetry: string
	translation: string
	srTime: number | string
	cnfLimit: number | string
}

interface Values {
	config: SRConfig
}

export const SRStage = (props: Props & FormikProps<Values> & FieldProps<any>) => {
	const { index, values } = props
	const { name } = props.field

	return (
		<StageHeader title='Savile Row' id={`sr${index + 1}`} isCollapsed={true}>
			<Field
				name={`${name}.optimisation`}
				component={SelectWithLabel}
				title='Optimisation'
				options={[
					{ value: '', label: 'Default' },
					{ value: '-O0', label: '0' },
					{ value: '-O1', label: '1' },
					{ value: '-O2', label: '2' },
					{ value: '-O3', label: '3' }
				]}
				values={values.config.optimisation}
			/>

			<Field
				name={`${name}.symmetry`}
				component={SelectWithLabel}
				title='Symmetry Breaking'
				options={[
					{ value: '', label: 'Default' },
					{ value: '-S0', label: '0' },
					{ value: '-S1', label: '1' },
					{ value: '-S2', label: '2' }
				]}
				values={values.config.symmetry}
			/>

			<Field
				name={`${name}.translation`}
				component={SelectWithLabel}
				title='Translation'
				options={[
					{ value: '', label: 'Default' },
					{ value: '-no-cse', label: 'No CSE' },
					{ value: '-identical-cse', label: 'Identical CSE' },
					{ value: '-ac-cse', label: 'AC CSE' },
					{ value: '-active-cse', label: 'Active CSE' },
					{ value: '-active-ac-cse', label: 'Active AC CSE' },
					{ value: '-deletevars', label: 'Delete Vars' },
					{ value: '-reduce-domains', label: 'Reduce Domains' },
					{
						value: '-reduce-domains-extend',
						label: 'Reduce Domains Extend'
					},
					{ value: '-aggregate', label: 'Aggregate' },
					{ value: '-tabulate', label: 'Tabulate' },
					{ value: '-nomappers', label: 'No Mappers' },
					{ value: '-minionmappers', label: 'Minion Mappers' },
					{ value: '-no-bound-vars', label: 'No Bound Variables' },
					{
						value: '-remove-redundant-vars',
						label: 'Remove Redundant Vars'
					},
					{
						value: '-var-sym-breaking',
						label: 'Variable Symmetry Breaking'
					}
				]}
				values={values.config.translation}
			/>
			<Field name={`${name}.srTime`} component={TextWithLabel} title='Time limit' values={values.config.srTime} />
			<Field
				name={`${name}.cnfLimit`}
				component={TextWithLabel}
				title='CNF clause limit'
				values={values.config.cnfLimit}
			/>
		</StageHeader>
	)
}
