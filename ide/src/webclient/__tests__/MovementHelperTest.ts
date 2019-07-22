import { WhichTree } from "../src/modules/Node"
import {
  coreOf3 as coreOf3Normal,
  coreOf17 as coreOf17Normal,
  coreOf27 as coreOf27Normal,
  descendantsOf28 as descendantsOf28Normal,
  core as coreNormal
} from "./resources/normal-8"
import {
  coreOf3 as coreOf3Sacbounds,
  coreOf6 as coreOf6Sacbounds,
  coreOf9 as coreOf9Sacbounds,
  core as coreSacbounds
} from "./resources/sacbounds-8"
import { loadDiffs } from "../src/modules/ForestHelper"
import { diffLocations } from "./resources/normalVSSacbounds-8"
import { flipDiffLocations } from "../src/modules/Helper"
import { goLeftBoyo } from "../src/modules/MovementHelper";

describe("test goleftboyo", () => {
  let bigTree: any
  let smallTree: any
  let flipped = flipDiffLocations(diffLocations)

  it("Checks that go left boyo inserts the nodes correctly into the tree", async () => {
    fetchMock.resetMocks()
    fetchMock
      .once(JSON.stringify(coreOf3Normal))
      .once(JSON.stringify(coreOf17Normal))
      .once(JSON.stringify(coreOf27Normal))
      .once(JSON.stringify(coreOf3Sacbounds))
      .once(JSON.stringify(coreOf6Sacbounds))
      .once(JSON.stringify(coreOf9Sacbounds))
      .once(JSON.stringify(descendantsOf28Normal))

    let res = await loadDiffs(
      ["", "s"],
      [coreNormal, coreSacbounds],
      diffLocations,
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
