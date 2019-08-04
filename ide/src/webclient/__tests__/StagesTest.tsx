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
import { render, fireEvent, queryByLabelText } from "@testing-library/react"
import "@testing-library/jest-dom/extend-expect"
import { MinionStage } from "../src/components/config/Stages"

describe("Test the stages components", () => {
  describe("Test the minion stage", () => {
    // const minionStage = getMinionStage(0)
    const index = 0

    const initialValues = {
      nodeLimit: 500
    }

    const minionStage = (
      <Formik
        initialValues={initialValues}
        onSubmit={values => {}}
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
    const { queryByText, getByLabelText, getByText } = rendered

    test("blah", () => {
      expect(queryByText("Minion")).toBeTruthy()

      console.log(
        getByLabelText("Node Limit", {
          selector: "input"
        }).outerHTML
      )
      // expect(queryByText("Node Limit")).toHave

      expect(
        getByLabelText("Node Limit", {
          selector: "input"
        })
      ).toHaveValue("500")
    })
  })
})
