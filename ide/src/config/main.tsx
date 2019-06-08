import * as React from "react"
import * as ReactDOM from "react-dom"
import FormikConjure from "./components/FormikConjure"

interface State {
    diff: boolean
    models: string[]
    params: string[]
}

class F extends React.Component<any, State>{

    constructor(props: any) {
        super(props)
        this.state = {
            diff: false,
            models: [],
            params: []
        }
        this.handler = this.handler.bind(this)
    }

    handler() {
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
            console.log(data)
            console.log({diff: false, ...data})
            this.setState({diff: false, ...data})
        })

    }

    render() {
        return (
            <div>

                <div className="input-group mb-3">
                    <div className="input-group-prepend">
                        <div className="input-group-text">
                            <input
                                name="isGoing"
                                type="checkbox"
                                checked={this.state.diff}
                                onChange={this.handler} />
                        </div>
                    </div>

                    <label className="form-control">
                    Diff two configurations
                    </label>
                </div>
                <FormikConjure
                    diff={this.state.diff}
                    essenceFiles={this.state.models}
                    paramFiles={this.state.params}
                />
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