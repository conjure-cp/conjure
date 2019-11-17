// import Select from "react-select"
import Select, { Option, OptGroup } from 'rc-select';
import * as React from "react"
import { Cache } from "../../../../extension/src/utils";

if (process.env.NODE_ENV !== "production") {
  const whyDidYouRender = require("@welldone-software/why-did-you-render/dist/no-classes-transpile/umd/whyDidYouRender.min.js")
  whyDidYouRender(React)
}

interface Props {
  caches: Cache[]
  index: number
  onChangeHandler: (cache: Cache, index: number) => void
}


export class Caches extends React.Component<Props, any> {

  render() {

    const options = this.props.caches.map((cache, index) => {
      return <Option value={index}>{cache.name}</Option>
    })

    return (
      <div className="row">
        <label className="col">Selected Cache</label>
        <div className="col">
          <Select
            defaultValue={<Option value={-1}>{"untitled"}</Option>
            }
          >
            {options}
          </Select>
        </div>
      </div>
    )
  }
}
