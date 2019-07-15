import * as React from "react"
import * as ReactDOM from "react-dom"
import StageHeader from "./components/StageHeader"
import FormikConjure from "./components/FormikConjure"
import { Check } from "./components/Check"
import { Cache, RepMap } from "../configHelper"
import { cloneDeep } from "lodash"
import Forest from "./components/Forest"
import "./styles.css"
import { InitResponse } from "../server/server";

if (process.env.NODE_ENV !== "production") {
  const whyDidYouRender = require("@welldone-software/why-did-you-render/dist/no-classes-transpile/umd/whyDidYouRender.min.js")
  whyDidYouRender(React)
}

interface State {
  trees: any
  isCollapsed: boolean
  diff: boolean
  allCaches: Cache[]
  selectedCaches?: (Cache | undefined)[]
  essenceFiles: string[]
  paramFiles: string[]
  reps: RepMap
  nimServerPort: number
  vscodeServerPort: number
}

class Root extends React.Component<any, State> {
  // static whyDidYouRender = true;

  constructor(props: any) {
    super(props)
    this.state = {
      trees: undefined,
      isCollapsed: false,
      diff: false,
      allCaches: [],
      paramFiles: [],
      essenceFiles: [],
      reps: {},
      nimServerPort: 5000,
      vscodeServerPort: Number(
        document.getElementById("port")!.getAttribute("vscodeserverport")
      )
    }
    console.log("hello")
  }

  collapseHandler = () => {
    this.setState((prevState: State) => {
      return { isCollapsed: !prevState.isCollapsed }
    })
  }

  initResponseHandler = (data: InitResponse) => {
    this.setState({
      isCollapsed: true,
      trees: data.trees,
      nimServerPort: data.nimServerPort
    })
    this.getFiles()
    console.log("The data from the vscodeserver")
    console.log(data)
  }

  cacheChangeHandler = (cache: Cache, index: number) => {
    this.setState((prevState: State) => {
      let copy = cloneDeep(prevState.selectedCaches)
      if (!copy) {
        copy = [undefined, undefined]
      }
      copy[index] = cache
      return { selectedCaches: copy }
    })
  }

  clickHandler = () => {
    this.setState((prevState: State) => {
      return { diff: !prevState.diff }
    })
  }

  getFiles = async () => {
    await fetch(`http://localhost:${this.state.vscodeServerPort}/config/files`)
      .then(response => response.json())
      .then(data => {
        this.setState({
          paramFiles: data.params,
          essenceFiles: data.models,
          reps: data.representations
        })
        return
      })
      .then(() => {
        fetch(`http://localhost:${this.state.vscodeServerPort}/config/caches`)
          .then(response => response.json())
          .then(data => {
            this.setState({
              allCaches: data
            })
            // console.log("fromServer", data)
          })
      })
  }

  componentDidMount = () => {
    this.getFiles()
  }

  render = () => {
    return (
      <div>
        <StageHeader
          title={"Setup"}
          id={"setup"}
          isCollapsed={this.state.isCollapsed}
          // isCollapsed={true}
          collapseHandler={this.collapseHandler}
        >
          <button
            className="btn btn-danger btn-lg btn-block mb-2"
            onClick={async () => {
              console.log("Should invalidate caches!")
              await fetch(
                `http://localhost:${this.state.vscodeServerPort}/config/invalidateCaches`
              )
              await this.getFiles()
              console.log("Done!!")
            }}
          >
            Invalidate Caches
          </button>

          <Check
            title={"Compare trees"}
            checked={this.state.diff}
            onChange={this.clickHandler}
          />

          <FormikConjure
            vscodeServerPort={this.state.vscodeServerPort}
            reps={this.state.reps}
            responseHandler={this.initResponseHandler}
            diff={this.state.diff}
            selectedCaches={this.state.selectedCaches}
            paramFiles={this.state.paramFiles}
            essenceFiles={this.state.essenceFiles}
            cacheChangeHandler={this.cacheChangeHandler}
            caches={this.state.allCaches}
          />
        </StageHeader>

        <Forest
          trees={this.state.trees}
          nimServerPort={this.state.nimServerPort}
        />

        {/* <TreeContainer info={"blah"} identifier={"letree"} core={testCore} /> */}
      </div>
    )
  }
}

ReactDOM.render(
  // <FormikApp email="barrybil@brownmail"/>,
  <div>
    <Root />
    {/* <FormikConjure diff={true}/> */}
  </div>,
  document.getElementById("root")
)
