import * as React from "react"
import * as ReactDOM from "react-dom"
import StageHeader from "./components/StageHeader"
import FormikConjure from "./components/FormikConjure"
import {Core, TreeVis} from "./components/TreeVis"


interface State {
    core: Core | undefined
    gotResponse: boolean
    diff: boolean
    models: string[]
    params: string[]
}

class F extends React.Component<any, State>{

    constructor(props: any) {
        super(props)
        this.state = {
            core: undefined,
            gotResponse: false,
            diff: false,
            models: [],
            params: []
        }
        this.clickHandler = this.clickHandler.bind(this)
        this.initResponseHandler = this.initResponseHandler.bind(this)
    }

    initResponseHandler(core: Core) {
        this.setState(
            (prevState: State) => {
                return { ...prevState, gotResponse: true, core: core }
            }
        )

        console.log("CALLED!")
        console.log(this.state.core)
        console.log("~~~~~~")
    }

    clickHandler() {
        this.setState(
            (prevState: State) => {
                return { diff: !prevState.diff }
            }
        )
    }

    componentDidMount() {
        fetch("http://localhost:4000/config/files")
            .then(response => response.json())
            .then((data) => {
                this.setState({ ...data })
            })
    }

    render() {
        return (
            <div>
                <StageHeader
                    title={"Setup"}
                    id={"Setup"}
                    // startCollapsed={this.state.gotResponse}
                    startCollapsed={false}
                >
                    <div className="input-group mb-3">
                        <div className="input-group-prepend">
                            <div className="input-group-text">
                                <input
                                    name="isGoing"
                                    type="checkbox"
                                    checked={this.state.diff}
                                    onChange={this.clickHandler} />
                            </div>
                        </div>

                        <label className="form-control">
                            Diff two configurations
                    </label>
                    </div>

                    <FormikConjure
                        responseHandler={this.initResponseHandler}
                        diff={this.state.diff}
                        essenceFiles={this.state.models}
                        paramFiles={this.state.params}
                    />
                </StageHeader>
                {this.state.core && <TreeVis core={this.state.core} />}
            </div>
        )
    }
}

ReactDOM.render(
    // <FormikApp email="barrybil@brownmail"/>,
    <div>
        <F />
        {/* <FormikConjure diff={true}/> */}
    </div>,
    document.getElementById("root")
)