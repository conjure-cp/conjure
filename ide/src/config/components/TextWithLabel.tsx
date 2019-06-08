import * as React from "react"
import * as ReactDOM from "react-dom"
import { FormikProps, FieldProps, Field, FormikErrors, getIn } from 'formik'
import Error from "./Error"

interface Props {
    label: string
}

interface Vals {
}

const TextWithLabel = (props: Props & FormikProps<Vals> & FieldProps<any>) => {
    const { touched, errors } = props.form 
    const { name } = props.field

    return (
        <div className="row">
            <div className="col">
                <label>
                    {props.label}
                </label>
            </div>
            <div className="col">
                <input
                    type="text"
                    className="input-group mb-3 tbox"
                    placeholder="None"
                    {...props.field}
                />
                <Error message={getIn(errors, name)} />
            </div>
        </div>

    )
}

export default TextWithLabel