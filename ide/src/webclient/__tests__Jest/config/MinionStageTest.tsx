import React from "react"

import {
  Form,
  Field,
  Formik} from "formik"
import {
  render} from "@testing-library/react"
import "@testing-library/jest-dom/extend-expect"
import { MinionStage } from "../../src/components/config/MinionStage"

describe("Test the stages components", () => {
  describe("Test the minion stage with values", () => {
    // const minionStage = getMinionStage(0)
    const index = 0

    const initialValues = {
      config: {
        nodeLimit: 1,
        solLimit: 1,
        minionTime: 1,
        minionSwitches: ["-findallsols", "-randomiseorder"],
        preprocessing: "GAC",
        consistency: "GAC"
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
    const { queryByText, getByLabelText } = rendered

    expect(queryByText("Minion")).toBeTruthy()



    test("node limit", () => {
      expect(
        getByLabelText("Node limit", {
          selector: "input"
        })
      ).toHaveValue(String(initialValues.config.nodeLimit))
    })

    test("sol limit", () => {
      expect(
        getByLabelText(/Solution/, {
          selector: "input"
        })
      ).toHaveValue(String(initialValues.config.solLimit))
    })

    test("minion time", () => {
      expect(
        getByLabelText(/CPU/, {
          selector: "input"
        })
      ).toHaveValue(String(initialValues.config.minionTime))
    })

    test("findallsols", () => {
      const checkBox = getByLabelText("Find all solutions", {
        selector: "input"
      })
      expect(checkBox).toHaveAttribute("checked", "")
    })

    test("randomiseorder", () => {
      const checkBox = getByLabelText("Randomise Var Order", {
        selector: "input"
      })
      expect(checkBox).toHaveAttribute("checked", "")
    })


    test("Preprocessing", () => {
        expect(getByLabelText(/Preprocessing/)).toHaveValue("GAC")
    })

    test("Consistency", () => {
        expect(getByLabelText(/Consistency/)).toHaveValue("GAC")
    })
  })
})
