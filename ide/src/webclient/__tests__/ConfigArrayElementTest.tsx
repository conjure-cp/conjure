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
  RenderResult,
  getByLabelText,
  wait
} from "@testing-library/react"
import "@testing-library/jest-dom/extend-expect"
import {
  ConfigArrayElement,
  Values
} from "../src/components/config/ConfigArrayElement"
import { RepMap, newCache } from "../../extension/src/utils"

describe("Test the configure element", () => {
  const findAllCache = newCache()
  findAllCache.name = "16all"
  findAllCache.config.minionConfig.minionSwitches = ["-findallsols"]
  findAllCache.essenceFile = "PartitionProblem.essence"
  findAllCache.paramFile = "16.param"

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

  const mockHandler = jest.fn(() => {
    console.log("mock handler called!")
  })

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
              caches={[findAllCache]}
              changeHandler={mockHandler}
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

  test("load cache", async () => {
    fireEvent.change(rendered.getByTestId("Caches-select"), {
      target: { value: findAllCache.name }
    })

    await wait(() => {})

    expect(mockHandler.mock.calls.length).toEqual(1)

    // expect(
    //   rendered.getByLabelText(/Model/, {
    //     selector: "input"
    //   })
    // ).toHaveValue(findAllCache.essenceFile)
  })
})
