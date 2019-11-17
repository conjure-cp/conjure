import * as React from "react"
import TextWithLabel from "./TextWithLabel"
import SelectWithLabel from "./SelectWithLabel"
import StageHeader from "../common/StageHeader"
import {Caches} from "../config/Caches"
import { Cache, RepMap } from "../../../../extension/src/utils"

import {
  Field,
  FieldProps,
  FormikProps
} from "formik"
import { MinionConfig, MinionStage } from "./MinionStage"
import { SRConfig, SRStage } from "./SRStage"
import { ConjureConfig, ConjureStage } from "./ConjureStage"
// import Select from "../common/Select"
// import { SelectField } from "./SelectField"
import { ChangeEvent } from "react";

interface Props {
  modelToReps: RepMap
  essenceFiles: string[]
  paramFiles: string[]
  index: number
  caches: Cache[]
  changeHandler: (e: ChangeEvent<HTMLSelectElement>) => void
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
    const { values, index } = this.props
    const { name } = this.props.field

    // console.log(values)
    return (
      <StageHeader
        isCollapsed={false}
        title={`Config ${index + 1}`}
        id={`config${index + 1}`}
      >
        <Field
          name={`${name}.name`}
          component={TextWithLabel}
          title={"Save as:"}
        />
        <Caches
        index={0}
        caches={this.props.caches}
        onChangeHandler={() => {}}

        
        />
        {/* <Field
          name={`${name}.Caches`}
          component={TextWithLabel}
          title={"Caches"}
        /> */}
        {/* <Field
          component={Select}
          title={"Caches"}
          options={this.props.caches.map(x => {
            return { value: x.name, label: x.name }
          })}
                  // changeHandler={(event: any) => {
                  //   const cacheName = event.target.value
                  //   const chosen = this.props.caches.find(
                  //     x => x.name === cacheName
                  //   )!

                  //   // console.log(event.target.value)
                  //   // console.log(cacheName)
                  //   // console.log(chosen)
                  //   currentCache = chosen
                  //   console.log(currentCache)
                  //   // this.setState({ currentCache })
                  // }}
        /> */}

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
