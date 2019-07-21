export const headers = {
  Accept: "application/json, text/plain, */*",
  "Content-Type": "text/plain"
}

export const flipDiffLocations = (diffLocations: number[][]) => {
  return diffLocations.map(x => [x[1], x[0]])
}
