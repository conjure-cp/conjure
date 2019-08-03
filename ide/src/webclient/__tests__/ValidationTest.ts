import { cleanCache } from "../src/components/config/Validation"
import { RepMap } from "../../extension/src/utils"

describe("Test the config validation module", () => {
  describe("Test the clean cache function", () => {
    test("With only model and param file", () => {
      let cache = {
        config: {
          paramFile: "set_partition_simple-params/8.param",
          essenceFile: "set_partition_simple.essence",
          conjureTime: "99999",
          strategy: "",
          optimisation: "-O2",
          symmetry: "-S2",
          translation: "-active-cse",
          srTime: "1111",
          minionTime: "6666",
          cnfLimit: "3333",
          minionSwitches: ["-findallsols", "-randomiseorder"],
          nodeLimit: "4444",
          solLimit: "5555",
          consistency: "SSAC",
          preprocessing: "SSAC",
          answers: ["1", "2", "2"]
        },
        name: "11:21:19_Config1"
      }

      let map: any = []

      map["set_partition_simple.essence"] = [
        { name: "n", representations: [{ description: "", answer: 1 }] },
        { name: "setA", representations: [{ description: "", answer: 1 }] },
        { name: "setB", representations: [{ description: "", answer: 1 }] },
      ]

      map = map as RepMap

      let res = cleanCache(cache, map, 0)

      expect(res.config.answers).toEqual(["n:1", "setA:2", "setB:2"])
      expect(res.name).toContain("Config1")

      let again = cleanCache(res, map, 1)
      expect(again.name).toContain("Config2")
    })
  })
})
