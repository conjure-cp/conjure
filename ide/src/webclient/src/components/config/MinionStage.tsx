import * as React from "react"
import * as ReactDOM from "react-dom"

import {
  Form,
  Field,
  FieldArray,
  FieldProps,
  Formik,
  FormikProps
} from "formik"
import * as Yup from "yup"

import { maxBy, times, isEqual, cloneDeep } from "lodash"
import TextWithLabel from "./TextWithLabel"
import SelectWithLabel from "./SelectWithLabel"
import StageHeader from "../common/StageHeader"
import Checkbox from "./Checkbox"
import { Check } from "../common/Check"
import { defaultProps } from "react-select/lib/Creatable"

interface Props {
  index: number
}

export interface MinionConfig {
  nodeLimit: number | string
  solLimit: number | string
  minionTime: number | string
  preprocessing: string
  consistency: string
  minionSwitches: string[]
}

interface Values {
  config: MinionConfig
}

export const MinionStage = (
  props: Props & FormikProps<Values> & FieldProps<any>
) => {
  const { index, values } = props
  const { name } = props.field

  let consistencyOptions = [
    { value: "", label: "Default" },
    { value: "GAC", label: "GAC" },
    { value: "SACBounds", label: "SACBounds" },
    { value: "SAC", label: "SAC" },
    { value: "SSACBounds", label: "SSACBounds" },
    { value: "SSAC", label: "SSAC" }
  ]

  return (
    <StageHeader title="Minion" id={`minion${index + 1}`} isCollapsed={true}>
      <Checkbox
        name={`${name}.minionSwitches`}
        value="-findallsols"
        title="Find all solutions"
        values={values.config.minionSwitches}
      />
      <Checkbox
        name={`${name}.minionSwitches`}
        value="-randomiseorder"
        title="Randomise Var Order"
        values={values.config.minionSwitches}
      />
      <Field
        name={`${name}.nodeLimit`}
        component={TextWithLabel}
        values={values.config.nodeLimit}
        title="Node limit"
      />
      <Field
        name={`${name}.solLimit`}
        component={TextWithLabel}
        values={values.config.solLimit}
        title="Solution limit"
      />
      <Field
        name={`${name}.minionTime`}
        component={TextWithLabel}
        values={values.config.minionTime}
        title="CPU limit"
      />

      <Field
        name={`${name}.preprocessing`}
        component={SelectWithLabel}
        title="Preprocessing"
        values={values.config.preprocessing}
        options={consistencyOptions}
      />
      <Field
        name={`${name}.consistency`}
        component={SelectWithLabel}
        title="Consistency"
        values={values.config.consistency}
        options={consistencyOptions}
      />
    </StageHeader>
  )
}
