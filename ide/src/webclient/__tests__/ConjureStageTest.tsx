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
  getByTestId
} from "@testing-library/react"
import "@testing-library/jest-dom/extend-expect"
import { ConjureStage } from "../src/components/config/ConjureStage"

describe("Test the stages components", () => {
  describe("Test the conjure stage with values", () => {
    // const minionStage = getMinionStage(0)
    const mockHandler = jest.fn(() => {console.log("mock handler called!")})
    const index = 0

    const varReps = [
      {
        name: "n",
        representations: [{ "1": "occurrence" }, { "2": "explicit" }]
      }
    ]

    const initialValues = {
      config: {
        conjureTime: 1,
        answers: [undefined],
        strategy: "c"
      }
    }

    const savileRowStage = (
      <Formik
        initialValues={initialValues}
        onSubmit={_values => {}}
        render={({ values }) => (
          <Form data-test-id="form">
            <Field
                    name={`config`}
                    component={ConjureStage}
                    index={index}
                    values={values}
                    varRepresentations={varReps}
                    showReps={[false]}
                    showRepsHandler={mockHandler}
            />
          </Form>
        )}
      ></Formik>
    )

    const rendered = render(savileRowStage)
    const {
      queryByText,
      getByLabelText,
      getByText,
      getByTestId,
      getByDisplayValue
    } = rendered

    expect(queryByText("Conjure")).toBeTruthy()

    test("Time limit", () => {
      expect(
        getByLabelText("Time limit", {
          selector: "input"
        })
      ).toHaveValue(String(initialValues.config.conjureTime))
    })

    test("Strategy", () => {
      expect(
        getByLabelText("Strategy", {
          selector: "input"
        })
      ).toHaveValue(String(initialValues.config.strategy))
    })

    test("checking", async () => {
        fireEvent.click(getByText("Choose Representation"))
        expect(mockHandler.mock.calls.length).toEqual(1)

    })
  })
})
