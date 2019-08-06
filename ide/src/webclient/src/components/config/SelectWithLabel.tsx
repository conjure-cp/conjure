import * as React from "react"
import { FormikProps, FieldProps, Field } from "formik"

interface Props {
  title: string
  options: { value: string; label: string }[]
}

const SelectWithLabel = (props: Props & FormikProps<any> & FieldProps<any>) => {
  const { touched, errors } = props.form
  const { name } = props.field

  // console.log(props.values)
  // console.log(props.field.value)

  let opts = props.options.map((option: Props["options"][0], i) => (
    <option key={i} value={option.value}>
      {option.label}
    </option>
  ))

  return (
    <div>
      <label htmlFor={`${props.title}-select`}>{props.title}</label>
      <Field
        id={`${props.title}-select`}
        className="browser-default custom-select mb-4"
        component="select"
        {...props.field}
        value={props.values}
      >
        {opts}
      </Field>
    </div>
  )
}

export default SelectWithLabel