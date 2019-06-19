import * as React from "react";
import StageHeader from "./StageHeader";

interface Props {
  id: string;
  selected: number;
}

interface Variable {
  name: string;
  rng: string;
}

interface Set extends Variable {
  // cardinali
}

interface Simple {
  vars: Variable[];
  changedNames: string[];
}

interface Pretty {}

interface State {
  vars: Variable[];
  changedNames: string[];
  pretty: boolean;
}

export class Domains extends React.Component<Props, State> {
  // static whyDidYouRender = true;

  constructor(props: Props) {
    super(props);
    this.state = {
      vars: [],
      changedNames: [],
      pretty: false
    };

    this.clickHandler = this.clickHandler.bind(this);
  }

  async getDomains() {
    const response = await fetch(
      `http://localhost:5000/${
        this.state.pretty ? "pretty" : "simple"
      }Domains/${this.props.selected}/false${this.state.pretty ? "/" : ""}`
    );
    const data = await response.json();
    console.log(data);
    this.setState({ vars: data.vars, changedNames: data.changedNames });
  }

  componentDidMount() {
    this.getDomains();
  }

  async componentDidUpdate(prevProps: Props, _prevState: State) {
    if (
      this.props.selected !== prevProps.selected ||
      this.props.id !== prevProps.id
    ) {
      await this.getDomains();
    }
  }

  async clickHandler() {
    this.setState(
      (prevState: State) => {
        return { pretty: !prevState.pretty };
      },
      async () => await this.getDomains()
    );
  }

  getRows() {
    console.log(this.state);
    // this.getDomains();
    return this.state.vars.map((variable, i) => {
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
      );
    });
  }

  render() {
    return (
      //   <h1>{this.state.changedNames[0]}</h1>
      <StageHeader title={"Domains"} id={"Domains"}>
        <div className="input-group mb-3">
          <div className="input-group-prepend">
            <div className="input-group-text">
              <input
                type="checkbox"
                checked={this.state.pretty}
                onChange={this.clickHandler}
              />
            </div>
          </div>
          <label className="form-control">Show pretty domains</label>
        </div>

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
    );
  }
}
