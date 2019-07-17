import Node from "../modules/Node"
import { makeState } from "../modules/TreeHelper"

test("adds numbers", () => {
  expect(2 + 2).toEqual(4)
  let s = makeState(
    { nodes: [new Node(0, "", "", -1, 0, true, 0, false)], solAncestorIds: [] },
    0
  )
  console.log(s)
})
