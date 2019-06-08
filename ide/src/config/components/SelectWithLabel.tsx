import * as React from "react"
import * as ReactDOM from "react-dom"
import { FormikProps, FieldProps, Field } from 'formik'

interface Props {
    label: string
    options: { val: string, text: string }[]
}

interface Vals {
    options: { val: string, text: string }[]
}

const SelectWithLabel = (props: Props & FormikProps<Vals> & FieldProps<any>) => {
    const { touched, errors } = props.form
    const { name } = props.field

    let opts = props.options.map(
        (option: Props["options"][0]) =>
            <option key={option.val} value={option.val}> {option.text} </option>
    )

    return (
        <div>
            <label>{props.label}
            </label>
            <Field
                className="browser-default custom-select mb-4"
                component="select"
                {...props.field}
            >
                {opts}
            </Field>
        </div>
    )
}

export default SelectWithLabel