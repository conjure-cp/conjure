import { cleanCache } from "../src/components/config/Validation"
import { RepMap } from "../../extension/src/utils"
import { newCache } from "../src/components/config/FormikConjure"

describe("Test the config validation module", () => {
  describe("Test the clean cache function", () => {
    test("With only model and param file", () => {
      const cache = newCache()
      let res = cleanCache(cache, 0)
      expect(res.name).toContain("Config1")
      let again = cleanCache(res, 1)
      expect(again.name).toContain("Config2")
      expect(again.name).not.toContain("Config1")
    })
  })
})
