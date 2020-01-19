import React from "react"

import {
  Form,
  Field,
  Formik} from "formik"
import {
  render,
  fireEvent,
  cleanup,
  RenderResult
} from "@testing-library/react"
import "@testing-library/jest-dom/extend-expect"
import { ConjureStage } from "../../src/components/config/ConjureStage"

describe("Test the stages components", () => {
  afterEach(cleanup)

  const index = 0
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

  const mockHandler = jest.fn(() => {
    console.log("mock handler called!")
  })

  describe("Test the conjure stage no reps", () => {
    // const minionStage = getMinionStage(0)
    const initialValues = {
      config: {
        conjureTime: 1,
        answers: [undefined],
        strategy: "s"
      }
    }

    let rendered: RenderResult

    beforeEach(() => {
      cleanup()

      const conjureStage = (
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
                showReps={false}
                showRepsHandler={mockHandler}
              />
            </Form>
          )}
        ></Formik>
      )

      rendered = render(conjureStage)
    })

    test("Title", () => {
      expect(rendered.queryByText("Conjure")).toBeTruthy()
    })

    test("Time limit", () => {
      expect(
        rendered.getByLabelText("Time limit", {
          selector: "input"
        })
      ).toHaveValue(String(initialValues.config.conjureTime))
    })

    test("Strategy", () => {
      expect(
        rendered.getByLabelText("Strategy", {
          selector: "input"
        })
      ).toHaveValue(String(initialValues.config.strategy))
    })

    test("checking", async () => {
      fireEvent.click(rendered.getByText("Choose Representation"))
      expect(mockHandler.mock.calls.length).toEqual(1)
    })
  })

  // cleanup()

  describe("Test the conjure stage reps", () => {
    // const minionStage = getMinionStage(0)
    const initialValues = {
      config: {
        answers: ["setA:2", "setB:2"]
      }
    }

    let rendered: RenderResult

    beforeEach(() => {
      const conjureStage = (
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
                showReps={true}
                showRepsHandler={() => {}}
              />
            </Form>
          )}
        ></Formik>
      )

      rendered = render(conjureStage)
    })

    test("Conjure", async () => {
      expect(rendered.queryByText("Conjure")).toBeTruthy()
    })

    test("checking", async () => {
      expect(rendered.getByLabelText("setA")).toBeTruthy()
      expect(rendered.getByLabelText("setB")).toBeTruthy()
    })

    test("setA", () => {
      expect(rendered.getByLabelText("setA")).toHaveValue(
        initialValues.config.answers[0]
      )
    })

    test("setB", () => {
      expect(rendered.getByLabelText("setB")).toHaveValue(
        initialValues.config.answers[1]
      )
    })
  })
})
