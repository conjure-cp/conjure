import * as React from "react";
import * as ReactDOM from "react-dom";

import { Form, Field, FieldArray, Formik, FieldArrayRenderProps } from "formik";
import * as Yup from "yup";

import TextWithLabel from "./TextWithLabel";
import SelectWithLabel from "./SelectWithLabel";
import StageHeader from "./StageHeader";
import Checkbox from "./Checkbox";

interface Values {
  configs: Config[];
}

interface Props {
  diff: boolean;
  essenceFiles: string[];
  paramFiles: string[];
  responseHandler: (content: any) => void;
}

interface Config {
  essenceFile: string;
  paramFile: string;
  strategy: string;
  optimisation: string;
  symmetry: string;
  translation: string;
  minionSwitches: string[];
  nodeLimit: number | string;
  solLimit: number | string;
  cpuLimit: number | string;
  conjureTime: number | string;
  srTime: number | string;
  minionTime: number | string;
  cnfLimit: number | string;
  consistency: string;
  preprocessing: string;
  [key: string]: string | number | string[];
}

const makeEmptyConfig = (props: Props): Config => ({
  paramFile: props.paramFiles[0],
  essenceFile: props.essenceFiles[0],
  conjureTime: "",
  strategy: "",
  optimisation: "",
  symmetry: "",
  translation: "",
  srTime: "",
  minionTime: "",
  cnfLimit: "",
  minionSwitches: [],
  nodeLimit: "",
  cpuLimit: "",
  solLimit: "",
  consistency: "",
  preprocessing: ""
});

const positiveInt = Yup.number()
  .positive()
  .integer()
  .moreThan(0);

const intOrNothing = Yup.mixed().test(
  "Is a valid number or empty string",
  "Leave empty or specify an integer > 0",
  (value: any) => {
    if (value === "") {
      return true;
    }
    return positiveInt.isValidSync(value);
  }
);

const schema = {
  conjureTime: intOrNothing,
  srTime: intOrNothing,
  minionTime: intOrNothing,
  cnfLimit: intOrNothing,
  nodeLimit: intOrNothing,
  cpuLimit: intOrNothing,
  solLimit: intOrNothing
};

const validationSchema = Yup.object().shape({
  configs: Yup.array().of(Yup.object().shape(schema))
});

const submissionHandler = (values: Values, props: Props) => {
  let cleaned = values.configs.map((config: Config) => {
    let cleaned: any = {};

    Object.keys(config).map((key: string) => {
      if (config[key] !== "") {
        cleaned[key] = config[key];
      }
    });

    if (cleaned["minionSwitches"].length === 0) {
      delete cleaned["minionSwitches"];
    }

    return cleaned;
  });

  fetch("http://localhost:4000/config/solve", {
    method: "post",
    headers: {
      Accept: "application/json, text/plain, */*",
      "Content-Type": "application/json"
    },
    body: JSON.stringify(cleaned)
  })
    .then(response => response.json())
    .then(data => {
      props.responseHandler(data.core);
      // console.log(data)
    })
    .catch(error => {
      console.error(error);
    });
};

const renderArrayElements = (props: Props, values: Values) =>
  values.configs.map((_config, index) => {
    return (
      <div className="col" key={index}>
        <StageHeader
          title={`Configuration ${index + 1}`}
          id={`config${index + 1}`}
        >
          <Field
            name={`configs[${index}].essenceFile`}
            component={SelectWithLabel}
            label="Model"
            options={props.essenceFiles.map(file => {
              return { val: file, text: file };
            })}
          />

          <Field
            name={`configs[${index}].paramFile`}
            component={SelectWithLabel}
            label="Param"
            options={props.paramFiles.map(file => {
              return { val: file, text: file };
            })}
          />

          <StageHeader
            title="Conjure"
            id={`conjure${index + 1}`}
            startCollapsed={true}
          >
            <Field
              name={`configs[${index}].conjureTime`}
              component={TextWithLabel}
              label={"Time limit"}
            />

            <Field
              name={`configs[${index}].strategy`}
              component={SelectWithLabel}
              label="Strategy"
              options={[
                { val: "", text: "Default" },
                { val: "c", text: "compact" },
                { val: "s", text: "sparse" }
              ]}
            />
          </StageHeader>

          <StageHeader
            title="Savilerow"
            id={`sr${index + 1}`}
            startCollapsed={true}
          >
            <Field
              name={`configs[${index}].optimisation`}
              component={SelectWithLabel}
              label="Optimisation"
              options={[
                { val: "", text: "Default" },
                { val: "-O0", text: "0" },
                { val: "-O1", text: "1" },
                { val: "-O2", text: "2" },
                { val: "-O3", text: "3" }
              ]}
            />
            <Field
              name={`configs[${index}].symmetry`}
              component={SelectWithLabel}
              label="Symmetry Breaking"
              options={[
                { val: "", text: "Default" },
                { val: "-S0", text: "0" },
                { val: "-S1", text: "1" },
                { val: "-S2", text: "2" }
              ]}
            />
            <Field
              name={`configs[${index}].translation`}
              component={SelectWithLabel}
              label="Translation"
              options={[
                { val: "", text: "Default" },
                { val: "-no-cse", text: "No CSE" },
                { val: "-identical-cse", text: "Identical CSE" },
                { val: "-ac-cse", text: "AC CSE" },
                { val: "-active-cse", text: "Active CSE" },
                { val: "-active-ac-cse", text: "Active AC CSE" },
                { val: "-deletevars", text: "Delete Vars" },
                { val: "-reduce-domains", text: "Reduce Domains" },
                {
                  val: "-reduce-domains-extend",
                  text: "Reduce Domains Extend"
                },
                { val: "-aggregate", text: "Aggregate" },
                { val: "-tabulate", text: "Tabulate" },
                { val: "-nomappers", text: "No Mappers" },
                { val: "-minionmappers", text: "Minion Mappers" },
                { val: "-no-bound-vars", text: "No Bound Variables" },
                {
                  val: "-remove-redundant-vars",
                  text: "Remove Redundant Vars"
                },
                { val: "-var-sym-breaking", text: "Variable Symmetry Breaking" }
              ]}
            />
            <Field
              name={`configs[${index}].srTime`}
              component={TextWithLabel}
              label="Time limit"
            />
            <Field
              name={`configs[${index}].cnfLimit`}
              component={TextWithLabel}
              label="CNF clause limit"
            />
          </StageHeader>

          <StageHeader
            title="Minion"
            id={`minion${index + 1}`}
            startCollapsed={true}
          >
            <Checkbox
              name={`configs[${index}].minionSwitches`}
              value="-findallsols"
              label="Find all solutions"
            />
            <Checkbox
              name={`configs[${index}].minionSwitches`}
              value="-randomiseorder"
              label="Randomise Var Order"
            />
            <Field
              name={`configs[${index}].nodeLimit`}
              component={TextWithLabel}
              label="Node Limit"
            />
            <Field
              name={`configs[${index}].solLimit`}
              component={TextWithLabel}
              label="Solution Limit"
            />
            <Field
              name={`configs[${index}].cpuLimit`}
              component={TextWithLabel}
              label="CPU Limit"
            />
            <Field
              name={`configs[${index}].preprocessing`}
              component={SelectWithLabel}
              label="Preprocessing"
              options={[
                { val: "", text: "Default" },
                { val: "GAC", text: "GAC" },
                { val: "SACBounds", text: "SACBounds" },
                { val: "SAC", text: "SAC" },
                { val: "SSACBounds", text: "SSACBounds" },
                { val: "SSAC", text: "SSAC" }
              ]}
            />
            <Field
              name={`configs[${index}].consistency`}
              component={SelectWithLabel}
              label="Consistency"
              options={[
                { val: "", text: "Default" },
                { val: "GAC", text: "GAC" },
                { val: "SACBounds", text: "SACBounds" },
                { val: "SAC", text: "SAC" },
                { val: "SSACBounds", text: "SSACBounds" },
                { val: "SSAC", text: "SSAC" }
              ]}
            />
          </StageHeader>
        </StageHeader>
      </div>
    );
  });

const Stage = (props: Props) => {
  let empty = makeEmptyConfig(props);

  let list = props.diff ? [empty, empty] : [empty];

  return (
    <Formik
      initialValues={{ configs: list }}
      onSubmit={values => {
        submissionHandler(values, props);
      }}
      validationSchema={validationSchema}
      enableReinitialize={true}
      render={({ values }) => (
        <Form>
          <div className="row">
            <FieldArray
              name="configs"
              render={() => renderArrayElements(props, values)}
            />
          </div>

          <button type="submit" className="btn btn-primary btn-lg btn-block">
            Solve
          </button>
        </Form>
      )}
    ></Formik>
  );
};

export default Stage;
