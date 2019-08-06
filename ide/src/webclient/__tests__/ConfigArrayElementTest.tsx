import React from "react"
import * as ReactDOM from "react-dom"

import {
  Form,
  Field,
  FieldArray,
  FieldProps,
  Formik,
  FormikProps
} from "formik"
import {
  render,
  fireEvent,
  waitForElement,
  queryByLabelText,
  getByTestId,
  cleanup,
  RenderResult
} from "@testing-library/react"
import "@testing-library/jest-dom/extend-expect"
import {
  ConfigArrayElement,
  Values
} from "../src/components/config/ConfigArrayElement"
import { RepMap } from "../../extension/src/utils"

describe("Test the configure element", () => {
  afterEach(cleanup)

  const varReps = [
    {
      name: "setA",
      representations: [
        { answer: "setA:1", description: "occurrence" },
        { answer: "setA:2", description: "explicit" }
      ]
    },
    {
      name: "setB",
      representations: [
        { answer: "setB:1", description: "occurrence" },
        { answer: "setB:2", description: "explicit" }
      ]
    }
  ]

  let essenceFileToReps: RepMap = {}
  const essenceFile = "blah.essence"
  const paramFile = "p.param"

  essenceFileToReps[essenceFile] = varReps

  // const minionStage = getMinionStage(0)
  const initialValues: Values = {
    cache: {
      name: "",
      essenceFile: essenceFile,
      paramFile: paramFile,
      config: {
        conjureConfig: {
          conjureTime: "",
          strategy: "",
          answers: [undefined, undefined]
        },
        srConfig: {
          optimisation: "",
          symmetry: "",
          translation: "",
          srTime: "",
          cnfLimit: ""
        },
        minionConfig: {
          minionTime: "",
          nodeLimit: "",
          solLimit: "",
          preprocessing: "",
          consistency: "",
          minionSwitches: []
        }
      }
    }
  }

  let rendered: RenderResult

  beforeEach(() => {
    cleanup()

    const configArrayElement = (
      <Formik
        initialValues={initialValues}
        onSubmit={_values => {}}
        render={({ values }) => (
          <Form data-test-id="form">
            <Field
              name={`cache`}
              component={ConfigArrayElement}
              modelToReps={essenceFileToReps}
              essenceFiles={[essenceFile]}
              paramFiles={[paramFile]}
              index={0}
              values={values}
            />
          </Form>
        )}
      ></Formik>
    )

    rendered = render(configArrayElement)
  })

  test("config 1 is displayed", () => {
    expect(rendered.queryByText("Config 1")).toBeTruthy()
  })

  test("click to show reps", () => {
    expect(rendered.queryByLabelText("Strategy")).toBeTruthy()
    fireEvent.click(rendered.getByText("Choose Representation"))
    expect(rendered.queryByLabelText("Strategy")).toBeFalsy()
    expect(rendered.queryByLabelText("setA")).toBeTruthy()
    expect(rendered.queryByLabelText("setB")).toBeTruthy()
  })
})
