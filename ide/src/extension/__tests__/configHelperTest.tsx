import { cacheToArgs, newCache } from "../src/utils"

describe("Test the config helper module", () => {

  describe("Test the cache to args function", () => {
    let folderPath =
      "/home/tom/EssenceCatalog/problems/csplib-prob049/vscodeExtensionCache"

    test("With only model and param file", () => {
      let expected = [
        "solve",
        "/home/tom/EssenceCatalog/problems/csplib-prob049/set_partition_simple.essence",
        "/home/tom/EssenceCatalog/problems/csplib-prob049/set_partition_simple-params/8.param",
        "-o",
        "/home/tom/EssenceCatalog/problems/csplib-prob049/vscodeExtensionCache/10:55:47_Config1",
        "--savilerow-options",
        '"',
        '"',
        "--solver-options",
        '"-dumptreesql',
        "/home/tom/EssenceCatalog/problems/csplib-prob049/vscodeExtensionCache/10:55:47_Config1/out.db",
        '"'
      ]

      let cache = newCache()

      cache.name = "10:55:47_Config1"
      cache.essenceFile = "set_partition_simple.essence"
      cache.paramFile = "set_partition_simple-params/8.param"

      let res = cacheToArgs(cache, folderPath)

      expect(res).toEqual(expected)
    })

    test("With all options and reps", () => {
      const expected = [
        "solve",
        "/home/tom/EssenceCatalog/problems/csplib-prob049/set_partition_simple.essence",
        "/home/tom/EssenceCatalog/problems/csplib-prob049/set_partition_simple-params/8.param",
        "-o",
        "/home/tom/EssenceCatalog/problems/csplib-prob049/vscodeExtensionCache/11:21:19_Config1",
        "--limit-time=99999",
        "-aai",
        "--channelling=no",
        "--smart-filenames",
        "--responses-representation",
        "n:1,setA:2,setB:2",
        "--savilerow-options",
        '"',
        "-O2",
        "-S2",
        "-active-cse",
        "-timelimit",
        "1111",
        "-cnflimit",
        "3333",
        '"',
        "--solver-options",
        '"-dumptreesql',
        "/home/tom/EssenceCatalog/problems/csplib-prob049/vscodeExtensionCache/11:21:19_Config1/out.db",
        "-findallsols",
        "-randomiseorder",
        "-nodelimit",
        "4444",
        "-sollimit",
        "5555",
        "-cpulimit",
        "6666",
        "-preprocess",
        "SSAC",
        "-prop-node",
        "SSAC",
        '"'
      ]

      let cache = newCache()

      cache.name = "11:21:19_Config1"
      cache.config.conjureConfig.conjureTime = 99999
      cache.essenceFile = "set_partition_simple.essence"
      cache.paramFile = "set_partition_simple-params/8.param"
      cache.config.conjureConfig.answers = ["n:1", "setA:2", "setB:2"]
      cache.config.srConfig.optimisation = "-O2"
      cache.config.srConfig.symmetry = "-S2"
      cache.config.srConfig.translation = "-active-cse"
      cache.config.srConfig.srTime = 1111
      cache.config.srConfig.cnfLimit = 3333
      cache.config.minionConfig.minionSwitches = ["-findallsols", "-randomiseorder"]
      cache.config.minionConfig.nodeLimit = 4444
      cache.config.minionConfig.solLimit = 5555
      cache.config.minionConfig.minionTime = 6666
      cache.config.minionConfig.preprocessing = "SSAC"
      cache.config.minionConfig.consistency = "SSAC"

      let res = cacheToArgs(cache, folderPath)

      expect(res).toEqual(expected)
    })

    test("With all options no reps", () => {
      const expected = [
        "solve",
        "/home/tom/EssenceCatalog/problems/csplib-prob049/set_partition_simple.essence",
        "/home/tom/EssenceCatalog/problems/csplib-prob049/set_partition_simple-params/8.param",
        "-o",
        "/home/tom/EssenceCatalog/problems/csplib-prob049/vscodeExtensionCache/11:21:19_Config1",
        "--limit-time=99999",
        "-a",
        "c",
        "--savilerow-options",
        '"',
        "-O2",
        "-S2",
        "-active-cse",
        "-timelimit",
        "1111",
        "-cnflimit",
        "3333",
        '"',
        "--solver-options",
        '"-dumptreesql',
        "/home/tom/EssenceCatalog/problems/csplib-prob049/vscodeExtensionCache/11:21:19_Config1/out.db",
        "-findallsols",
        "-randomiseorder",
        "-nodelimit",
        "4444",
        "-sollimit",
        "5555",
        "-cpulimit",
        "6666",
        "-preprocess",
        "SSAC",
        "-prop-node",
        "SSAC",
        '"'
      ]

      let cache = newCache()

      cache.name = "11:21:19_Config1"
      cache.config.conjureConfig.conjureTime = 99999
      cache.config.conjureConfig.strategy = "c"
      cache.essenceFile = "set_partition_simple.essence"
      cache.paramFile = "set_partition_simple-params/8.param"
      cache.config.srConfig.optimisation = "-O2"
      cache.config.srConfig.symmetry = "-S2"
      cache.config.srConfig.translation = "-active-cse"
      cache.config.srConfig.srTime = 1111
      cache.config.srConfig.cnfLimit = 3333
      cache.config.minionConfig.minionSwitches = ["-findallsols", "-randomiseorder"]
      cache.config.minionConfig.nodeLimit = 4444
      cache.config.minionConfig.solLimit = 5555
      cache.config.minionConfig.minionTime = 6666
      cache.config.minionConfig.preprocessing = "SSAC"
      cache.config.minionConfig.consistency = "SSAC"

      let res = cacheToArgs(cache, folderPath)

      expect(res).toEqual(expected)
    })
  })
})
