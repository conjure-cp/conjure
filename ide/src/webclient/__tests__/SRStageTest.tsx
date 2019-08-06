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
  queryByLabelText,
  getByTestId
} from "@testing-library/react"
import "@testing-library/jest-dom/extend-expect"
import { SavileRowStage } from "../src/components/config/SRStage"

describe("Test the stages components", () => {
  describe("Test the saville row stage with values", () => {
    // const minionStage = getMinionStage(0)
    const index = 0

    const initialValues = {
      config: {
        srTime: 1,
        cnfLimit: 1,
        optimisation: "-O1",
        symmetry: "-S0"
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
              component={SavileRowStage}
              index={index}
              values={values}
            />
          </Form>
        )}
      ></Formik>
    )

    const rendered = render(savileRowStage)
    const { queryByText, getByLabelText, getByText, getByTestId, getByDisplayValue } = rendered

    expect(queryByText("Savile Row")).toBeTruthy()


    test("Optimisation", () => {
        expect(getByLabelText(/Optimisation/)).toHaveValue("-O1")
    })

    test("Symmetry", () => {
        expect(getByLabelText(/Symmetry/)).toHaveValue("-S0")
    })

    test("Time limit", () => {
      expect(
        getByLabelText("Time limit", {
          selector: "input"
        })
      ).toHaveValue(String(initialValues.config.srTime))
    })

    test("CNF limit", () => {
      expect(
        getByLabelText("CNF clause limit", {
          selector: "input"
        })
      ).toHaveValue(String(initialValues.config.cnfLimit))
    })
  })
})
