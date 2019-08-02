import { DiffPoint } from "../components/Forest"

export const headers = {
  Accept: "application/json, text/plain, */*",
  "Content-Type": "text/plain"
}

export const flipDiffLocations = (diffLocations: DiffPoint[]) => {
  return diffLocations.map(x => {
    return {
      leftTreeId: x.rightTreeId,
      rightTreeId: x.leftTreeId,
      highlightLeft: x.highlightRight,
      highlightRight: x.highlightLeft,
      descCount: x.descCount,
      path: "SADASDAS"
    }
  })
}
