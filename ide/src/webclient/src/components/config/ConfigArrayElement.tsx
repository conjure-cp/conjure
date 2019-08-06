import * as React from "react"
import TextWithLabel from "./TextWithLabel"
import SelectWithLabel from "./SelectWithLabel"
import StageHeader from "../common/StageHeader"
import { Check } from "../common/Check"
import { validationSchema } from "./Validation"
import { Cache, RepMap } from "../../../../extension/src/utils"

import {
  Form,
  Field,
  FieldArray,
  FieldProps,
  Formik,
  FormikProps
} from "formik"
import { MinionConfig, MinionStage } from "./MinionStage"
import { SRConfig, SRStage } from "./SRStage"
import { ConjureConfig, ConjureStage } from "./ConjureStage"
import { cloneDeep } from "lodash"

interface Props {
  modelToReps: RepMap
  essenceFiles: string[]
  paramFiles: string[]
  index: number
}

interface State {
  showReps: boolean
}

export interface CombinedConfig {
  conjureConfig: ConjureConfig
  srConfig: SRConfig
  minionConfig: MinionConfig
}

export interface Values {
  cache: Cache
}

export class ConfigArrayElement extends React.Component<
  Props & FormikProps<Values> & FieldProps<any>,
  State
> {
  state = {
    showReps: false
  }
  render = () => {

    //   console.log(this.state)
      
    const { values, index } = this.props
    const { name } = this.props.field
    return (
      <StageHeader
        isCollapsed={false}
        title={`Config ${index + 1}`}
        id={`config${index + 1}`}
      >
        <Field
          name={`${name}.essenceFile`}
          component={SelectWithLabel}
          title="Model"
          options={this.props.essenceFiles.map(file => {
            return { value: file, label: file }
          })}
          values={values.cache.essenceFile}
        />

        <Field
          name={`${name}.paramFile`}
          component={SelectWithLabel}
          title="Parameter"
          options={this.props.paramFiles.map(file => {
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
            })
          }
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
