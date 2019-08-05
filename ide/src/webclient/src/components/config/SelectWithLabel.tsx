import * as React from "react"
import { FormikProps, FieldProps, Field } from "formik"

interface Props {
  title: string
  options: { value: string; label: string }[]
}

const SelectWithLabel = (props: Props & FormikProps<any> & FieldProps<any>) => {
  const { touched, errors } = props.form
  const { name } = props.field

  let opts = props.options.map((option: Props["options"][0]) => (
    <option key={option.value} value={option.value}>
      {option.label}
    </option>
  ))

  return (
    <div>
      <label>
        {props.title}
        <Field
          data-testid={props.title}
          className="browser-default custom-select mb-4"
          component="select"
          {...props.field}
          value={props.values}
        >
          {opts}
        </Field>
      </label>
    </div>
  )
}

export default SelectWithLabel
