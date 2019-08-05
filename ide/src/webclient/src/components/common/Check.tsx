import * as React from "react"

interface Props {
  checked: boolean
  onChange: () => void
  title: string
}

export const Check = (props: Props) => {
  return (
    <div className="input-group mb-3">
      <div className="input-group-prepend">
        <div className="input-group-text">
          <input
            id={`${props.title}-Check`}
            type="checkbox"
            checked={props.checked}
            onChange={props.onChange}
          />
        </div>
      </div>
      <label className="form-control" htmlFor={`${props.title}-Check`}>
        {props.title}
      </label>
    </div>
  )
}
