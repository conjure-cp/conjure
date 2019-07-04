import * as React from "react"
import { Form, Field, FieldArray, Formik } from "formik"
import Select from "react-select"
import { defaultProps } from "react-select/lib/Select"

export default class MySelect extends React.Component<any> {
  handleChange = (value: any) => {
    // this is going to call setFieldValue and manually update values.topcis

    this.props.onChange(
      `namedConfigs[${this.props.configIndex}].config.answers[${this.props.index}]`,
      value
    )
  }

  //   handleBlur = () => {
  //     // this is going to call setFieldTouched and manually update touched.topcis
  //     this.props.onBlur(`answers[${this.props.name}]`, false)
  //   }

  render() {
    return (
      <div style={{ margin: "1rem 0" }}>
        <label htmlFor="color">Topics (select at least 3) </label>
        <Select
          id="color"
          options={this.props.options}
          //   multi={true}
          onChange={this.handleChange}
          //   onBlur={this.handleBlur}
          value={this.props.value}
          defaultValue={this.props.options[0]}
        />
        {!!this.props.error && this.props.touched && (
          <div style={{ color: "red", marginTop: ".5rem" }}>
            {this.props.error}
          </div>
        )}
      </div>
    )
  }
}
