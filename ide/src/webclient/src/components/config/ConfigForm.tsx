import * as React from "react"
import { Check } from "../common/Check"
import { validationSchema } from "./Validation"
import { Cache, RepMap, newCache } from "../../../../extension/src/utils"

import { Form, Field, Formik, FormikProps, validateYupSchema } from "formik"

import { cloneDeep } from "lodash"
import { ConfigArrayElement } from "./ConfigArrayElement"

var Loader = require("react-loader")

// import { MDBSpinner } from "mdbreact"

// import Loader from "react-loader-spinner"
// import "react-loader-spinner/dist/loader/css/react-spinner-loader.css"

interface Props {
  caches: Cache[]
  waiting: boolean
  modelToReps: RepMap
  essenceFiles: string[]
  paramFiles: string[]
  submitHandler: (values: Values) => void
}

interface State {
  diff: boolean
  // currentCache?: Cache
}

interface Values {
  caches: Cache[]
}

export class ConfigForm extends React.Component<Props, State> {
  state = {
    diff: false,
    currentCache: undefined
  }

  render = () => {
    const initialCache = {
      ...newCache(),
      essenceFile: this.props.essenceFiles[0],
      paramFile: this.props.paramFiles[0]
    }

    const initialValues = { caches: [initialCache, cloneDeep(initialCache)] }

    return (
      <Formik
        initialValues={initialValues}
        onSubmit={values => {
          this.props.submitHandler(values)
        }}
        validationSchema={validationSchema}
        isInitialValid={() =>
          validateYupSchema(initialValues, validationSchema, true) !== undefined
        }
        enableReinitialize={true}
        render={(renderProps: FormikProps<Values>) => {
          const values = renderProps.values

          const submitButtonMessage = renderProps.isValid
            ? "Solve"
            : "Fix the errors first!"

          const colourClass = renderProps.isValid ? "primary" : "warning"
          const arrayIndexes = this.state.diff ? [0, 1] : [0]

          const array = arrayIndexes.map(index => {
            let currentCache: Cache
            if (this.state.currentCache) {
              currentCache = this.state.currentCache!
            } else {
              currentCache = values.caches[index]
            }
            console.log("currentCahces is ", currentCache)

            return (
              <div className="col" key={index}>
                <Field
                  name={`caches[${index}]`}
                  component={ConfigArrayElement}
                  modelToReps={this.props.modelToReps}
                  essenceFiles={this.props.essenceFiles}
                  paramFiles={this.props.paramFiles}
                  index={index}
                  caches={this.props.caches}
                  values={{ cache: currentCache }}
                  // changeHandler={(event: any) => {
                  //   const cacheName = event.target.value
                  //   const chosen = this.props.caches.find(
                  //     x => x.name === cacheName
                  //   )!

                  //   // console.log(event.target.value)
                  //   // console.log(cacheName)
                  //   // console.log(chosen)
                  //   currentCache = chosen
                  //   console.log(currentCache)
                  //   // this.setState({ currentCache })
                  // }}
                />
              </div>
            )
          })

          return (
            <Form>
              <Check
                title={"Compare trees"}
                checked={this.state.diff}
                onChange={() => {
                  this.setState((prevState: State) => {
                    return { diff: !prevState.diff }
                  })
                }}
              />

              <div className="row">{array}</div>

              <Loader loaded={!this.props.waiting}>
                <button
                  type="submit"
                  className={`btn btn-${colourClass} btn-lg btn-block`}
                >
                  {submitButtonMessage}
                </button>
              </Loader>
            </Form>
          )
        }}
      ></Formik>
    )
  }
}
