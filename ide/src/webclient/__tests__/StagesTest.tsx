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
import { MinionStage } from "../src/components/config/Stages"

describe("Test the stages components", () => {
  describe("Test the minion stage with values", () => {
    // const minionStage = getMinionStage(0)
    const index = 0

    const initialValues = {
      config: {
        nodeLimit: 1,
        solLimit: 1,
        minionTime: 1,
        preprocessing: 1,
        consistency: 1,
        minionSwitches: []
      }
    }

    const minionStage = (
      <Formik
        initialValues={initialValues}
        onSubmit={_values => {}}
        render={({ values }) => (
          <Form>
            <Field
              name={`config`}
              component={MinionStage}
              index={index}
              values={values}
            />
          </Form>
        )}
      ></Formik>
    )

    const rendered = render(minionStage)
    const { queryByText, getByLabelText, getByText, getByTestId } = rendered

    expect(queryByText("Minion")).toBeTruthy()

    test("node limit", () => {
      expect(
        getByLabelText("Node Limit", {
          selector: "input"
        })
      ).toHaveValue(String(initialValues.config.nodeLimit))
    })

    test("sol limit", () => {
      expect(
        getByLabelText("Solution Limit", {
          selector: "input"
        })
      ).toHaveValue(String(initialValues.config.solLimit))
    })

    test("minion time", () => {
      expect(
        getByLabelText("CPU Limit", {
          selector: "input"
        })
      ).toHaveValue(String(initialValues.config.minionTime))
    })
  })
})
