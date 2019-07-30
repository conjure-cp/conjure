import { WhichTree } from "../src/modules/Node"
import {
  coreOf3 as coreOf3Normal,
  coreOf7 as coreOf7Normal,
  coreOf17 as coreOf17Normal,
  coreOf21 as coreOf21Normal,
  coreOf27 as coreOf27Normal,
  descendantsOf28 as descendantsOf28Normal,
  core as coreNormal8
} from "./resources/normal-8"
import {
  coreOf3 as coreOf3Sacbounds,
  coreOf4 as coreOf4Sacbounds,
  coreOf6 as coreOf6Sacbounds,
  coreOf7 as coreOf7Sacbounds,
  coreOf9 as coreOf9Sacbounds,
  core as coreSacbounds8
} from "./resources/sacbounds-8"
import { loadAllDiffs } from "../src/modules/ForestHelper"
import { bigToSmall } from "./resources/normalVSSacbounds-8"
import { flipDiffLocations } from "../src/modules/Helper"
import { goLeftBoyo } from "../src/modules/MovementHelper";

describe("test goleftboyo", () => {
  let bigTree: any
  let smallTree: any
  let flipped = flipDiffLocations(bigToSmall)

  it("Checks that go left boyo inserts the nodes correctly into the tree", async () => {
    fetchMock.resetMocks()
    fetchMock
      .once(JSON.stringify(coreOf3Normal))
      .once(JSON.stringify(coreOf3Sacbounds))
      .once(JSON.stringify(coreOf7Normal))
      .once(JSON.stringify(coreOf4Sacbounds))
      .once(JSON.stringify(coreOf17Normal))
      .once(JSON.stringify(coreOf6Sacbounds))
      .once(JSON.stringify(coreOf21Normal))
      .once(JSON.stringify(coreOf7Sacbounds))
      .once(JSON.stringify(coreOf27Normal))
      .once(JSON.stringify(coreOf9Sacbounds))

      .once(JSON.stringify(descendantsOf28Normal))

    let res = await loadAllDiffs(
      ["", "s"],
      [coreNormal8, coreSacbounds8],
      bigToSmall,
      5000
    )

    bigTree = res[0]
    bigTree = (await goLeftBoyo(
      28,
      bigTree,
      false,
      false,
      "",
      10,
      5000,
      WhichTree.Right
    )).id2Node

      smallTree = res[1]
      
    expect(bigTree[29]).toBeTruthy()
  })

    
})
