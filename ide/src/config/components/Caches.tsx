import * as React from "react";
import * as ReactDOM from "react-dom";
import { Core, TreeContainer, MyMap } from "./TreeContainer";
import { Form, Field, FieldArray, Formik } from "formik";
import { Cache } from "../../server/server";

if (process.env.NODE_ENV !== "production") {
  const whyDidYouRender = require("@welldone-software/why-did-you-render/dist/no-classes-transpile/umd/whyDidYouRender.min.js");
  whyDidYouRender(React);
}

interface Props {
  label: string;
  caches: Cache[];
  onChangeHandler: (cacheName: string) => void;
}

interface State {
  selectedOption: { value: string; label: string };
}

import Select from "react-select";

const options = [
  { value: "chocolate", label: "Chocolate" },
  { value: "strawberry", label: "Strawberry" },
  { value: "vanilla", label: "Vanilla" }
];

export class Caches extends React.Component<Props, State> {
  state = {
    selectedOption: { value: "", label: "" }
  };

  handleChange = (selectedOption: any) => {
    this.setState({ selectedOption: selectedOption });
    this.props.onChangeHandler(selectedOption.value);
    // console.log(`Option selected:`, selectedOption);
  };

  getOptions = () => {
    return this.props.caches.map(c => {
      return { value: c.config, label: c.timeStamp };
    });
  };

  render() {
    const { selectedOption } = this.state;

    return (
      <Select
        value={selectedOption}
        onChange={this.handleChange}
        options={this.getOptions()}
      />
    );
  }
}

// function change(event: any) {
//   console.log(event.target.value);
// }

// export const Caches = (props: Props) => {
//   let opts = props.caches.map(option => (
//     <option key={option.timeStamp} value={option.config} onChange={change}>
//       {option.timeStamp}
//     </option>
//   ));

//   return (
//     <div>
//       <label>{props.label}</label>
//       <select>{opts}</select>
//     </div>
//   );
// };

// export class Caches extends React.Component<Props, State> {
//   constructor(props: any) {
//     super(props);
//     this.state = {
//       cachedSelection: ""
//     };
//   }

//   render() {
//     return <div className="row"></div>;
//   }
// }
