import * as React from "react"

export const Wrapper = (props: any) => (
  <div
    style={{
      flex: 1,
      display: "flex",
      height: "100%",
      width: "100%",
      border: "1px solid red",
      position: "relative"
    }}
  >
    {props.children}
  </div>
)
