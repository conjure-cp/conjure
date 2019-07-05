import * as React from "react"
import Select from "react-select"
import { defaultProps } from "react-select/lib/Select"
import { FormikProps, FieldProps, Field, FormikErrors, getIn } from "formik"

interface Opt {
  label: string
  value: string | number
}

interface Props {
  onChange: (...args: any) => void
  options: Opt[]
  title: string
  name: string
  value: string | Opt
}

interface State {
  first: boolean
  selectedOption: Opt
}

export default class MySelect extends React.Component<Props, State> {
  constructor(props: Props) {
    super(props)
    // console.log(props.options)
    this.state = { first: true, selectedOption: props.options[0] }
  }

  componentDidUpdate(prevProps: Props) {
    if (this.props.value !== prevProps.value) {
      if (typeof this.props.value === "string") {
        this.setState({
          selectedOption: { label: this.props.value, value: this.props.value }
        })
      } else {
        this.setState({
          selectedOption: this.props.value
        })
      }
    }
  }

  render() {
    // console.log(this.state)

    return (
      <div style={{ margin: "1rem 0" }}>
        <label htmlFor="color"> {this.props.title} </label>
        <Select
          id="color"
          options={this.props.options}
          value={
            this.props.value !== "" && !this.state.selectedOption
              ? typeof this.props.value === "string"
                ? { label: this.props.value, value: this.props.value }
                : this.props.value
              : this.state.selectedOption
          }
          onChange={(option: any) => {
            this.setState({ selectedOption: option })
            this.props.onChange(this.props.name, option.value)
          }}
        />
      </div>
    )
  }
}
