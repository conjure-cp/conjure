import * as React from "react";
import * as ReactDOM from "react-dom";
import StageHeader from "./components/StageHeader";
import FormikConjure from "./components/FormikConjure";
import { Core, TreeContainer, MyMap } from "./components/TreeContainer";
import Node from "./modules/Node";

if (process.env.NODE_ENV !== "production") {
  const whyDidYouRender = require("@welldone-software/why-did-you-render/dist/no-classes-transpile/umd/whyDidYouRender.min.js");
  whyDidYouRender(React);
}

interface State {
  core: Core | undefined;
  gotResponse: boolean;
  diff: boolean;
  models: string[];
  params: string[];
}

class F extends React.Component<any, State> {
  static whyDidYouRender = true;

  constructor(props: any) {
    super(props);
    this.state = {
      core: undefined,
      gotResponse: false,
      diff: false,
      models: [],
      params: []
    };
    this.clickHandler = this.clickHandler.bind(this);
    this.initResponseHandler = this.initResponseHandler.bind(this);
    this.collapseHandler = this.collapseHandler.bind(this);
  }

  collapseHandler() {}

  initResponseHandler(core: Core) {
    this.setState({ gotResponse: true, core: core });
  }

  clickHandler() {
    this.setState((prevState: State) => {
      return { diff: !prevState.diff, gotResponse: false };
    });
  }

  componentDidMount() {
    fetch("http://localhost:4000/config/files")
      .then(response => response.json())
      .then(data => {
        this.setState({ ...data });
      });
  }

  render() {
    const testCore = {
      nodes: [
        {
          id: 0,
          parentId: -1,
          label: "",
          prettyLabel: "",
          childCount: 1,
          isSolution: false,
          isLeftChild: true,
          descCount: 32
        },
        {
          id: 1,
          parentId: 0,
          label: "Root Propagation",
          prettyLabel: "Root Propagation",
          childCount: 1,
          isSolution: false,
          isLeftChild: true,
          descCount: 31
        },
        {
          id: 2,
          parentId: 1,
          label: "setA_Occurrence_00001 = 0",
          prettyLabel: "1 was excluded from setA",
          childCount: 2,
          isSolution: false,
          isLeftChild: true,
          descCount: 30
        },
        {
          id: 16,
          parentId: 2,
          label: "setA_Occurrence_00002 != 0",
          prettyLabel: "2 was included in setA",
          childCount: 2,
          isSolution: false,
          isLeftChild: false,
          descCount: 16
        },
        {
          id: 26,
          parentId: 16,
          label: "setA_Occurrence_00003 != 0",
          prettyLabel: "3 was included in setA",
          childCount: 1,
          isSolution: false,
          isLeftChild: false,
          descCount: 6
        },
        {
          id: 27,
          parentId: 26,
          label: "setA_Occurrence_00004 = 0",
          prettyLabel: "4 was excluded from setA",
          childCount: 2,
          isSolution: false,
          isLeftChild: true,
          descCount: 5
        },
        {
          id: 30,
          parentId: 27,
          label: "setA_Occurrence_00005 != 0",
          prettyLabel: "5 was included in setA",
          childCount: 1,
          isSolution: false,
          isLeftChild: false,
          descCount: 2
        },
        {
          id: 31,
          parentId: 30,
          label: "setA_Occurrence_00006 = 0",
          prettyLabel: "6 was excluded from setA",
          childCount: 1,
          isSolution: false,
          isLeftChild: true,
          descCount: 1
        },
        {
          id: 32,
          parentId: 31,
          label: "setA_Occurrence_00007 = 0",
          prettyLabel: "7 was excluded from setA",
          childCount: 0,
          isSolution: true,
          isLeftChild: true,
          descCount: 0
        },
        {
          id: 3,
          parentId: 2,
          label: "setA_Occurrence_00002 = 0",
          prettyLabel: "2 was excluded from setA",
          childCount: 2,
          isSolution: false,
          isLeftChild: true,
          descCount: 12
        },
        {
          id: 17,
          parentId: 16,
          label: "setA_Occurrence_00003 = 0",
          prettyLabel: "3 was excluded from setA",
          childCount: 2,
          isSolution: false,
          isLeftChild: true,
          descCount: 8
        },
        {
          id: 28,
          parentId: 27,
          label: "setA_Occurrence_00005 = 0",
          prettyLabel: "5 was excluded from setA",
          childCount: 1,
          isSolution: false,
          isLeftChild: true,
          descCount: 1
        }
      ],
      solAncestorIds: [0, 1, 2, 16, 26, 27, 30, 31, 32],
      id: "blah"
    };

    return (
      <div className="row">
        <StageHeader
          title={"Setup"}
          id={"Setup"}
          // startCollapsed={this.state.gotResponse}
          collapseHandler={this.collapseHandler}
        >
          <div className="input-group mb-3">
            <div className="input-group-prepend">
              <div className="input-group-text">
                <input
                  type="checkbox"
                  checked={this.state.diff}
                  onChange={this.clickHandler}
                />
              </div>
            </div>

            <label className="form-control">Diff two configurations</label>
          </div>

          <FormikConjure
            responseHandler={this.initResponseHandler}
            diff={this.state.diff}
            essenceFiles={this.state.models}
            paramFiles={this.state.params}
          />
        </StageHeader>
        {this.state.core && <TreeContainer core={this.state.core} />}
        {/* <TreeContainer core={testCore} /> */}
      </div>
    );
  }
}

ReactDOM.render(
  // <FormikApp email="barrybil@brownmail"/>,
  <div>
    <F />
    {/* <FormikConjure diff={true}/> */}
  </div>,
  document.getElementById("root")
);
