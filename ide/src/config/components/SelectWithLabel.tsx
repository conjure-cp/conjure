import * as React from "react"
import * as ReactDOM from "react-dom"
import { FormikProps, FieldProps, Field } from "formik"

interface Props {
  label: string
  options: { val: string; text: string }[]
  // changeHandler: () => void
}

interface Vals {
  options: { val: string; text: string }[]
}

const SelectWithLabel = (props: Props & any) => {
  const { touched, errors } = props.form
  const { name } = props.field

  // let opts = props.options.map((option: Props["options"][0]) => (
  //   <option key={option.val} value={option.val}>
  //     {option.text}
  //   </option>
  // ))

  return (
    <div>
      <label>{props.label}</label>
      <Field
        className="browser-default custom-select mb-4"
        // component="select"
        {...props.field}
      >
        {({ field, form }: FieldProps) => (
          <select>
            {props.options.map((option: Props["options"][0]) => (
              <option
                key={option.val}
                value={option.val}
                onChange={() => {
                  console.log(field)
                  form.setFieldValue(props.name, field.value)
                }}
              >
                {option.text}
              </option>
            ))}
          </select>
        )}
      </Field>
    </div>
  )
}

// {({ field, form }: FieldProps) =>
//   props.options.map((option: Props["options"][0]) => (
//     <option
//       key={option.val}
//       value={option.val}
//       onChange={() => {
//         console.log(field)
//         form.setFieldValue(props.name, field.value)
//       }}
//     >
//       {option.text}
//     </option>
//   ))
// }
export default SelectWithLabel
