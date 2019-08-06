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
  diff: boolean
  diffCheckHandler: (namedCache1: Cache) => void
  modelToReps: RepMap
  essenceFiles: string[]
  paramFiles: string[]
}

interface State {
  showReps: boolean[]
}

export interface CombinedConfig {
  conjureConfig: ConjureConfig
  srConfig: SRConfig
  minionConfig: MinionConfig
}

interface Values {
  caches: Cache[]
}

export class ConfigForm extends React.Component<Props, State> {
  state = {
    showReps: [false, false]
  }

  render = () => {
    return (
      <Formik
        initialValues={{
          caches: [
            {
              name: "",
              essenceFile: "",
              paramFile: "",
              config: {
                conjureConfig: { conjureTime: "", strategy: "", answers: [] },
                srConfig: {
                  optimisation: "",
                  symmetry: "",
                  translation: "",
                  srTime: "",
                  cnfLimit: ""
                },
                minionConfig: {
                  nodeLimit: "",
                  solLimit: "",
                  minionTime: "",
                  preprocessing: "",
                  consistency: "",
                  minionSwitches: []
                }
              }
            }
          ]
        }}
        onSubmit={values => {}}
        validationSchema={validationSchema}
        enableReinitialize={true}
        render={(renderProps: FormikProps<Values>) => {
          const values = renderProps.values
          const index = 0

          return (
            <Form>
              <Check
                title={"Compare trees"}
                checked={this.props.diff}
                onChange={() => {}}
              />

              <Field
                name={`values.caches[${index}].essenceFile`}
                component={SelectWithLabel}
                title="Model"
                options={this.props.essenceFiles.map(file => {
                  return { value: file, label: file }
                })}
                values={values.caches[index].essenceFile}
              />

              <Field
                name={`values.caches[${index}].paramFile`}
                component={SelectWithLabel}
                title="Parameter"
                options={this.props.paramFiles.map(file => {
                  return { value: file, label: file }
                })}
                values={values.caches[index].paramFile}
              />

              <Field
                name={`values.caches[${index}].config.conjureConfig`}
                Component={ConjureStage}
                values={values.caches[index].config.conjureConfig}
                index={index}
                varRepresentations={
                  this.props.modelToReps[values.caches[index].essenceFile]
                }
                showReps={this.state.showReps[index]}
                showRepsHandler={() =>
                  this.setState((prevState: State) => {
                    let copy = cloneDeep(prevState.showReps)
                    copy[index] = !copy[index]
                    return { showReps: copy }
                  })
                }
              />

              <Field
                name={`values.caches[${index}].config.srConfig`}
                Component={SRStage}
                values={values.caches[index].config.srConfig}
                index={index}
              />

              <Field
                name={`values.caches[${index}].config.minionConfig`}
                Component={MinionStage}
                values={values.caches[index].config.minionConfig}
                index={index}
              />

              <button
                type="submit"
                className="btn btn-primary btn-lg btn-block"
              >
                Solve
              </button>
            </Form>
          )
        }}
      ></Formik>
    )
  }
}
