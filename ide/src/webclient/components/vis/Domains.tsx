import * as React from "react"
import StageHeader from "../common/StageHeader"
import { Check } from "../common/Check"

interface Props {
  hash: string
  path: string
  selected: number
  nimServerPort: number
}

interface Variable {
  name: string
  rng: string
}

interface State {
  vars: Variable[]
  changedNames: string[]
  pretty: boolean
  onlyChanged: boolean
  collapsed: boolean
}

export class Domains extends React.Component<Props, State> {
  // static whyDidYouRender = true;

  constructor(props: Props) {
    super(props)
    this.state = {
      vars: [],
      changedNames: [],
      pretty: false,
      onlyChanged: false,
      collapsed: false
    }
  }

  async getDomains() {
    if (this.state.collapsed) {
      return
    }

    let payload = {
      path: this.props.path,
      nodeId: this.props.selected
    }

    const response = await fetch(
      `http://localhost:${this.props.nimServerPort}/simpleDomains`,
      {
        method: "post",
        headers: {
          Accept: "application/json, text/plain, */*",
          "Content-Type": "text/plain"
        },
        body: JSON.stringify(payload)
      }
    )

    const data = await response.json()

    this.setState({ vars: data.vars, changedNames: data.changedNames })
  }

  componentDidMount() {
    this.getDomains()
  }

  async componentDidUpdate(prevProps: Props, prevState: State) {
    if (
      this.props.selected !== prevProps.selected ||
      this.props.hash !== prevProps.hash ||
      this.state.collapsed !== prevState.collapsed
    ) {
      await this.getDomains()
    }
  }

  clickHandler = async () => {
    this.setState(
      (prevState: State) => {
        return { pretty: !prevState.pretty }
      },
      async () => await this.getDomains()
    )
  }

  collpaseHandler = () => {
    this.setState((prevState: State) => {
      return { collapsed: !prevState.collapsed }
    })
  }

  getRows() {
    return this.state.vars.map((variable, i) => {
      if (this.state.onlyChanged) {
        if (!this.state.changedNames.includes(variable.name)) {
          return
        }
      }

      return (
        <tr
          key={variable.name}
          className={
            this.state.changedNames.includes(variable.name) ? "changed" : ""
          }
        >
          <th scope="row">{i}</th>
          <td>{variable.name}</td>
          <td>{variable.rng}</td>
        </tr>
      )
    })
  }

  render() {
    return (
      //   <h1>{this.state.changedNames[0]}</h1>
      <StageHeader
        title={`Domains at ${this.props.selected}`}
        id={"Domains"}
        isCollapsed={true}
        collapseHandler={this.collpaseHandler}
      >
        <Check
          title="Show pretty domains"
          checked={false}
          onChange={() => console.log("toggled pretty domains")}
        />

        <Check
          title="Only show changed"
          checked={this.state.onlyChanged}
          onChange={() => {
            this.setState((prevState: State) => {
              return { onlyChanged: !prevState.onlyChanged }
            })
          }}
        />

        {!this.state.pretty ? (
          <div className="table-wrapper-scroll-y my-custom-scrollbar">
            <table className="table table-bordered table-striped mb-0">
              <thead>
                <tr>
                  <th scope="col">#</th>
                  <th scope="col">Name</th>
                  <th scope="col">Domain</th>
                </tr>
              </thead>
              <tbody>{this.getRows()}</tbody>
            </table>
          </div>
        ) : (
          <></>
        )}
      </StageHeader>
    )
  }
}
