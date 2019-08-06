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
  wait
} from "@testing-library/react"
import "@testing-library/jest-dom/extend-expect"
import { ConfigForm } from "../src/components/config/ConfigForm"
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

  let rendered: RenderResult

  const mockHandler = jest.fn(() => {
    console.log("mock handler called!")
  })

  beforeEach(() => {
    cleanup()

    rendered = render(
      <ConfigForm
        modelToReps={essenceFileToReps}
        essenceFiles={[essenceFile]}
        paramFiles={[paramFile]}
        submitHandler={mockHandler}
      />
    )
  })

  test("Solve button is displayed", () => {
    expect(rendered.queryByText("Solve")).toBeTruthy()
  })

  test("Click diff check", async () => {
    expect(rendered.queryByText("Config 2")).toBeFalsy()

    fireEvent.click(rendered.getByText("Compare trees"))
    await wait(() => {})

    expect(rendered.queryByText("Config 1")).toBeTruthy()
    expect(rendered.queryByText("Config 2")).toBeTruthy()
  })

  test("Click Solve button", async () => {
    // fireEvent.change(rendered.getByLabelText("Time limit"), {target: {value: 'a'}})
    // fireEvent.click(rendered.getByText("Conjure"))
    // fireEvent.click(rendered.getByText("Savile Row"))
    // fireEvent.click(rendered.getByText("Minion"))

    // fireEvent.change(rendered.queryByLabelText("Time limit")!, {target: {value: '1234'}})

    // fireEvent.click(rendered.getByText("Solve"))

    fireEvent.click(rendered.getByText("Solve"))
    await wait(() => {})
    expect(mockHandler.mock.calls.length).toEqual(1)
  })
})
