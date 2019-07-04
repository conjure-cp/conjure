import * as React from "react"
import { Form, Field, FieldArray, Formik } from "formik"
import Select from "react-select"
import { defaultProps } from "react-select/lib/Select"

interface Opt {
  label: string
  value: string | number
}

interface Props {
  onChange: (...args: any) => void
  options: Opt[]
  //   value: Opt
  //   default: Opt
  title: string
  name: string
}

interface State {
  first: boolean
}

export default class MySelect extends React.Component<Props, State> {
  constructor(props: Props) {
    super(props)
    this.state = { first: true }
  }

  handleChange = (value: any) => {
    this.setState({ first: false })
    this.props.onChange(this.props.name, value)
  }

  render() {
    // console.log("DEFAULT", this.props.options[0])

    return (
      <div style={{ margin: "1rem 0" }}>
        <label htmlFor="color"> {this.props.title} </label>
        <Select
          id="color"
          options={this.props.options}
          onChange={this.handleChange}
          value={this.state.first ? this.props.options[0] : undefined}
        />
      </div>
    )
  }
}
