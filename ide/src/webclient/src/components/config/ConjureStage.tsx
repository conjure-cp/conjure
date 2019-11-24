import * as React from "react"
import TextWithLabel from "./TextWithLabel"
import SelectWithLabel from "./SelectWithLabel"
import StageHeader from "../common/StageHeader"
import { Check } from "../common/Check"

import {
  Form,
  Field,
  FieldArray,
  FieldProps,
  Formik,
  FormikProps
} from "formik"

import { VarRepresentation } from "../../../../extension/src/utils"

interface Props {
  varRepresentations: VarRepresentation[]
  index: number
  showReps: boolean
  showRepsHandler: () => void
}

export interface ConjureConfig {
  conjureTime: number | string
  strategy: string
  answers: (string | undefined)[]
}

interface Values {
  config: ConjureConfig
}

export const ConjureStage = (
  props: Props & FormikProps<Values> & FieldProps<any>
) => {
  const { index, showReps, showRepsHandler, varRepresentations, values } = props
  const { name } = props.field

  //   console.log(values.config.answers)

  // console.log(props)



  const repSelectBoxes = varRepresentations.map((x, i) => {

    const options = x.representations.map(y => {
      return {
        value: y.answer,
        label: y.description
      }
    })

    // console.log(values.config.answers[i])
    return (
      <Field
        key={i}
        name={`${name}.answers[${i}]`}
        component={SelectWithLabel}
        title={x.name}
        options={options}
        values={values.config.answers[i]}
      />
    )
  })

  return (
    <StageHeader title="Conjure" id={`conjure${index + 1}`} isCollapsed={true}>

      <Field
        name={`${name}.conjureTime`}
        component={TextWithLabel}
        title={"Time limit"}
        values={values.config.conjureTime}
      />
      <>
        {!showReps && (
          <Field
            name={`${name}.strategy`}
            component={SelectWithLabel}
            title="Strategy"
            options={[
              { value: "", label: "Default" },
              { value: "c", label: "compact" },
              { value: "s", label: "sparse" }
            ]}
            values={values.config.strategy}
          />
        )}
      </>
      <Check
        title={"Choose Representation"}
        checked={showReps}
        onChange={showRepsHandler}
      />
      {showReps && repSelectBoxes}
      {/* {} */}
    </StageHeader>
  )
}
