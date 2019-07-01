import * as React from "react"
import { Caches } from "./Caches"
import * as ReactDOM from "react-dom"

import { Form, Field, FieldArray, Formik } from "formik"
import * as Yup from "yup"

import TextWithLabel from "./TextWithLabel"
import SelectWithLabel from "./SelectWithLabel"
import StageHeader from "./StageHeader"
import Checkbox from "./Checkbox"
import { Cache } from "../../configHelper"

interface Values {
  namedConfigs: Cache[]
}

interface Props {
  diff: boolean
  caches: Cache[]
  cacheChangeHandler: (cache: Cache, index: number) => void
  essenceFiles: string[]
  paramFiles: string[]
  selectedCaches?: (Cache | undefined)[]
  responseHandler: (content: any) => void
}

interface Config {
  essenceFile: string
  paramFile: string
  strategy: string
  optimisation: string
  symmetry: string
  translation: string
  minionSwitches: string[]
  nodeLimit: number | string
  solLimit: number | string
  cpuLimit: number | string
  conjureTime: number | string
  srTime: number | string
  minionTime: number | string
  cnfLimit: number | string
  consistency: string
  preprocessing: string
  [key: string]: string | number | string[]
}

const makeNamedConfig = (props: Props, index: number): Cache => {
  const getName = (caches?: (Cache | undefined)[]) => {
    if (!caches) {
      return ""
    }
    if (!caches[index]) {
      return ""
    }

    return caches[index]!.name
  }

  let name = getName(props.selectedCaches)
  // console.log(props.selectedCaches);
  // console.log("naeis", name);

  return {
    config: makeEmptyConfig(props, index),
    name: name
  }
}

const makeEmptyConfig = (props: Props, index: number): Config => {
  // console.log("selected cache args!", props.selectedCaches);

  let initialConfig: Config = {
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
  }

  if (!props.selectedCaches || !props.selectedCaches[index]) {
    return initialConfig
  }
  return overwriteWithCachedOptions(props.selectedCaches[index]!, initialConfig)
}

const overwriteWithCachedOptions = (
  selectedCache: Cache,
  initialConfig: Config
): Config => {
  // console.log("selected Cache ", selectedCache);

  Object.keys(initialConfig).map(key => {
    if (key in selectedCache.config) {
      initialConfig[key] = selectedCache.config[key]
    }
  })

  return initialConfig
}

const positiveInt = Yup.number()
  .positive()
  .integer()
  .moreThan(0)

const intOrNothing = Yup.mixed().test(
  "Is a valid number or empty string",
  "Leave empty or specify an integer > 0",
  (value: any) => {
    if (value === "") {
      return true
    }
    return positiveInt.isValidSync(value)
  }
)

const schema = {
  conjureTime: intOrNothing,
  srTime: intOrNothing,
  minionTime: intOrNothing,
  cnfLimit: intOrNothing,
  nodeLimit: intOrNothing,
  cpuLimit: intOrNothing,
  solLimit: intOrNothing
}

const validationSchema = Yup.object().shape({
  configs: Yup.array().of(Yup.object().shape(schema))
})

const submissionHandler = (values: Values, props: Props) => {
  let cleaned = values.namedConfigs.map((namedConfig, index) => {
    const config = namedConfig.config

    let cleaned: any = {}

    Object.keys(config).map((key: string) => {
      if (config[key] !== "") {
        cleaned[key] = config[key]
      }
    })

    if (cleaned["minionSwitches"].length === 0) {
      delete cleaned["minionSwitches"]
    }

    let newNamedConfig: Cache = {
      config: cleaned,
      name:
        namedConfig.name !== ""
          ? namedConfig.name
          : `${new Date()
              .toUTCString()
              .replace(/ /g, "_")
              .replace(/,/g, "_")}_${index + 1}`
    }

    return newNamedConfig
  })

  // console.log(cleaned);

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
      console.log("From formik", data)
      props.responseHandler(data)
    })
  // .catch(error => {
  //   console.log(error);
  // });
}

const renderArrayElements = (props: Props, values: Values) =>
  values.namedConfigs.map((_config, index) => {
    return (
      <div className="col" key={index}>
        <StageHeader
          isCollapsed={false}
          title={`Configuration ${index + 1}`}
          id={`config${index + 1}`}
        >
          <Field
            name={`namedConfigs[${index}].name`}
            component={TextWithLabel}
            label={"Save as:"}
          />

          <Caches
            index={index}
            label={"Caches"}
            caches={props.caches}
            onChangeHandler={props.cacheChangeHandler}
          />

          <Field
            name={`namedConfigs[${index}].config.essenceFile`}
            component={SelectWithLabel}
            label="Model"
            options={props.essenceFiles.map(file => {
              return { val: file, text: file }
            })}
          />

          <Field
            name={`namedConfigs[${index}].config.paramFile`}
            component={SelectWithLabel}
            label="Param"
            options={props.paramFiles.map(file => {
              return { val: file, text: file }
            })}
          />

          <StageHeader
            title="Conjure"
            id={`conjure${index + 1}`}
            isCollapsed={true}
          >
            <Field
              name={`namedConfigs[${index}].config.conjureTime`}
              component={TextWithLabel}
              label={"Time limit"}
            />

            <Field
              name={`namedConfigs[${index}].config.strategy`}
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
            isCollapsed={true}
          >
            <Field
              name={`namedConfigs[${index}].config.optimisation`}
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
              name={`namedConfigs[${index}].config.symmetry`}
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
              name={`namedConfigs[${index}].config.translation`}
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
              name={`namedConfigs[${index}].config.srTime`}
              component={TextWithLabel}
              label="Time limit"
            />
            <Field
              name={`namedConfigs[${index}].config.cnfLimit`}
              component={TextWithLabel}
              label="CNF clause limit"
            />
          </StageHeader>

          <StageHeader
            title="Minion"
            id={`minion${index + 1}`}
            isCollapsed={true}
          >
            <Checkbox
              name={`namedConfigs[${index}].config.minionSwitches`}
              value="-findallsols"
              label="Find all solutions"
            />
            <Checkbox
              name={`namedConfigs[${index}].config.minionSwitches`}
              value="-randomiseorder"
              label="Randomise Var Order"
            />
            <Field
              name={`namedConfigs[${index}].config.nodeLimit`}
              component={TextWithLabel}
              label="Node Limit"
            />
            <Field
              name={`namedConfigs[${index}].config.solLimit`}
              component={TextWithLabel}
              label="Solution Limit"
            />
            <Field
              name={`namedConfigs[${index}].config.cpuLimit`}
              component={TextWithLabel}
              label="CPU Limit"
            />
            <Field
              name={`namedConfigs[${index}].config.preprocessing`}
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
              name={`namedConfigs[${index}].config.consistency`}
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
    )
  })

const Stage = (props: Props) => {
  // console.log("props", props);

  let list = props.diff
    ? [makeNamedConfig(props, 0), makeNamedConfig(props, 1)]
    : [makeNamedConfig(props, 0)]

  return (
    <Formik
      initialValues={{ namedConfigs: list }}
      onSubmit={values => {
        submissionHandler(values, props)
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
  )
}

export default Stage
