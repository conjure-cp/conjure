import React from "react"

import {
    render, fireEvent
} from "@testing-library/react"
import "@testing-library/jest-dom/extend-expect"
import { Caches } from "../../src/components/config/Caches"
import { newCache } from "../../../extension/src/utils"

describe("Test the stages components", () => {
    describe("Test the minion stage with values", () => {

        const testCache = newCache()
        testCache.name = "TheTestCache"

        const caches = (
            <Caches caches={[testCache]}
                index={0}
                onChangeHandler={() => { }} />
        )

        const rendered = render(caches)
        const { getByText, getByTestId } = rendered

        test("Select box is rendered with default value", () => {
            expect(getByText("untitled")).toBeInTheDocument()
        })

        // test("Select box is rendered with default value", () => {
        //     fireEvent.change(getByTestId("select"), { target: { value: "green" } });

        //     expect(getByText("untitled")).toBeInTheDocument()
        // })
        // const minionStage = getMinionStage(0)
    })
})
