import * as React from "react";
import * as ReactDOM from "react-dom";
import StageHeader from "./components/StageHeader";
import FormikConjure from "./components/FormikConjure";
import { Core, TreeContainer, MyMap } from "./components/TreeContainer";
import { Form, Field, FieldArray, Formik } from "formik";
import { Caches } from "./components/Caches";
import SelectWithLabel from "./components/SelectWithLabel";
import { Cache } from "../server/server";

if (process.env.NODE_ENV !== "production") {
  const whyDidYouRender = require("@welldone-software/why-did-you-render/dist/no-classes-transpile/umd/whyDidYouRender.min.js");
  whyDidYouRender(React);
}

interface State {
  // core: Core | undefined;
  initResponse: any;
  gotResponse: boolean;
  diff: boolean;
  caches: Cache[];
  cachedConfig?: any;
  essenceFiles: string[];
  paramFiles: string[];
}

class F extends React.Component<any, State> {
  // static whyDidYouRender = true;

  constructor(props: any) {
    super(props);
    this.state = {
      initResponse: undefined,
      gotResponse: false,
      diff: false,
      caches: [],
      paramFiles: [],
      essenceFiles: []
      // selectedCacheArgs: {
      //   paramFiles: [],
      //   essenceFiles: []
      // }
    };
    this.clickHandler = this.clickHandler.bind(this);
    this.initResponseHandler = this.initResponseHandler.bind(this);
    this.collapseHandler = this.collapseHandler.bind(this);
    this.cacheSelectionHandler = this.cacheSelectionHandler.bind(this);
  }

  collapseHandler() {}

  initResponseHandler(data: any) {
    this.setState({ gotResponse: true, initResponse: data });
    // console.log(core);
  }

  cacheSelectionHandler(config: any) {
    this.setState({ cachedConfig: config });
  }

  clickHandler() {
    this.setState((prevState: State) => {
      return { diff: !prevState.diff, gotResponse: false };
    });
  }

  async componentDidMount() {
    await fetch("http://localhost:4000/config/files")
      .then(response => response.json())
      .then(data => {
        this.setState({
          paramFiles: data.params,
          essenceFiles: data.models
        });
        return;
      })
      .then(() => {
        fetch("http://localhost:4000/config/caches")
          .then(response => response.json())
          .then(data => {
            this.setState({
              caches: [{ timeStamp: "Untitled", config: {} }].concat(data)
            });
            console.log("fromServer", data);
          });
      });

    console.log(this.state);
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

    const fiveNodes = {
      nodes: [
        {
          id: 0,
          parentId: -1,
          label: "",
          prettyLabel: "",
          childCount: 2,
          isSolution: false,
          isLeftChild: true,
          descCount: 4
        },
        {
          id: 1,
          parentId: 0,
          label: "Root Propagation",
          prettyLabel: "Root Propagation",
          childCount: 1,
          isSolution: false,
          isLeftChild: true,
          descCount: 1
        },
        {
          id: 3,
          parentId: 0,
          label: "setA_Occurrence_00001 = 0",
          prettyLabel: "1 was excluded from setA",
          childCount: 1,
          isSolution: false,
          isLeftChild: false,
          descCount: 1
        },
        {
          id: 2,
          parentId: 1,
          label: "setA_Occurrence_00001 = 0",
          prettyLabel: "1 was excluded from setA",
          childCount: 0,
          isSolution: false,
          isLeftChild: false,
          descCount: 0
        },
        {
          id: 4,
          parentId: 3,
          label: "setA_Occurrence_00001 = 0",
          prettyLabel: "1 was excluded from setA",
          childCount: 0,
          isSolution: false,
          isLeftChild: true,
          descCount: 0
        }
      ],
      id: "poop",
      solAncestorIds: [0, 2]
    };

    return (
      <div className="row">
        {/* // <div> */}
        <StageHeader
          title={"Setup"}
          id={"Setup"}
          startCollapsed={this.state.gotResponse}
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

          <Caches
            label={"Caches"}
            caches={this.state.caches}
            onChangeHandler={this.cacheSelectionHandler}
          />

          <FormikConjure
            responseHandler={this.initResponseHandler}
            diff={this.state.diff}
            cachedConfig={this.state.cachedConfig}
            paramFiles={this.state.paramFiles}
            essenceFiles={this.state.essenceFiles}
          />
        </StageHeader>

        <div className="col">
          {this.state.initResponse && this.state.initResponse.core && (
            <TreeContainer
              core={this.state.initResponse.core}
              identifier={"tree1"}
            />
          )}
        </div>
        <div className="col">
          {this.state.initResponse && this.state.initResponse.core && (
            <TreeContainer
              core={this.state.initResponse.core}
              identifier={"tree2"}
            />
          )}
        </div>
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