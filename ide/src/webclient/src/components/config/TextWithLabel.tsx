import * as React from "react"
import * as ReactDOM from "react-dom"
import { FormikProps, FieldProps, Field, FormikErrors, getIn } from "formik"
import Error from "./Error"

interface Props {
  title: string
}

const TextWithLabel = (props: Props & FormikProps<any> & FieldProps<any>) => {
  const { touched, errors } = props.form
  const { name } = props.field

  // console.log("In text with label!!", props.values)

  return (
    <div className="row">
      <div className="col">
        <label htmlFor={`${props.title}-textBox`}>{props.title}</label>
        <input
          id={`${props.title}-textBox`}
          type="text"
          className="input-group mb-3 tbox"
          placeholder="None"
          {...props.field}
          value={props.values}
        />
        <Error message={getIn(errors, name)} />
      </div>
    </div>
  )
}

export default TextWithLabel
