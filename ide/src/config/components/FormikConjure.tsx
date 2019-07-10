import * as React from "react"
import { Caches } from "./Caches"
import * as ReactDOM from "react-dom"

import { Form, Field, FieldArray, Formik, FormikProps } from "formik"
import * as Yup from "yup"

import { maxBy, times, isEqual, cloneDeep } from "lodash"
import TextWithLabel from "./TextWithLabel"
import StageHeader from "./StageHeader"
import Checkbox from "./Checkbox"
import { Check } from "./Check"
import MySelect from "./MySelect"
import NewSelect from "./NewSelect"
import { Cache, VarRepresentation, RepMap } from "../../configHelper"
import { reporters } from "mocha"
import { prependListener } from "cluster"
import { type } from "os"
const shortid = require("shortid")

type RepChoices = Record<string, string>

interface Values {
  namedConfigs: Cache[]
}

interface Props {
  vscodeServerPort: number
  diff: boolean
  caches: Cache[]
  cacheChangeHandler: (cache: Cache, index: number) => void
  essenceFiles: string[]
  paramFiles: string[]
  reps: RepMap
  selectedCaches?: (Cache | undefined)[]
  responseHandler: (content: any) => void
}

interface Config {
  answers: any
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
  [key: string]: string | number | string[] | number[]
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

const configSchema = {
  conjureTime: intOrNothing,
  srTime: intOrNothing,
  minionTime: intOrNothing,
  cnfLimit: intOrNothing,
  nodeLimit: intOrNothing,
  cpuLimit: intOrNothing,
  solLimit: intOrNothing
}

const namedConfigSchema = {
  config: Yup.object().shape(configSchema),
  name: Yup.string().matches(
    /^$|^[a-z_:0-9]+$/i,
    "Name nust be alphanumeric(underscore/colons allowed), leave blank for timestamp"
  )
}

const validationSchema = Yup.object().shape({
  namedConfigs: Yup.array().of(Yup.object().shape(namedConfigSchema))
})

interface State {
  showReps: boolean[]
}

class ConfigForm extends React.Component<Props, State> {
  constructor(props: Props) {
    super(props)
    this.state = { showReps: [false, false] }
  }

  submissionHandler = (values: Values, props: Props, state: State) => {
    // console.log("!!!!!!!!!!!!!")
    // console.log(values)

    let cleaned = values.namedConfigs.map((namedConfig, index) => {
      const config = namedConfig.config

      let cleaned: any = {}

      Object.keys(config).map((key: string) => {
        if (config[key] !== "") {
          cleaned[key] = config[key]
        }
      })

      if (config.minionSwitches) {
        cleaned["minionSwitches"] = config.minionSwitches
      }

      // console.log(config.answers)

      cleaned.answers = config.answers.map(
        (answer: number | string, i: number) => {
          if (typeof answer !== "number" && answer !== "") {
            return
          }

          let variable = this.props.reps[cleaned.essenceFile][i]
          return `${variable.name}:${answer !== "" ? answer : 1}`
        }
      )

      // cleaned.answers = cleaned.answers.filter((x: any) => x !== undefined)
      //  || cleaned.answers.length === 0
      if (!state.showReps[index]) {
        delete cleaned["answers"]
      }

      let newNamedConfig: Cache = {
        config: cleaned,
        name:
          namedConfig.name !== ""
            ? namedConfig.name
            : `${new Date()
                .toUTCString()
                .replace(/ /g, "_")
                .replace(/,/g, "_")}_Config${index + 1}_${shortid.generate()}`
      }

      return newNamedConfig
    })

    console.log(cleaned)

    fetch(`http://localhost:${this.props.vscodeServerPort}/config/solve`, {
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
  }

  renderArrayElements = (props: Props, values: Values, setFieldValue: any) => {
    console.log(values)

    return values.namedConfigs.map((_config, index) => {
      const currentEssenceFile = values.namedConfigs[index].config.essenceFile

      const varReps = currentEssenceFile ? props.reps[currentEssenceFile] : []

      const repSelectBoxes = varReps.map((vR, i) => {
        // values.namedConfigs[index].config.answers[i] =
        //   values.namedConfigs[index].config.answers[i] === ""
        //     ? vR.representations[0].answer
        //     : values.namedConfigs[index].config.answers[i]

        const cachedChoice = vR.representations.find(
          x => x.answer === values.namedConfigs[index].config.answers[i]
        )

        return (
          <NewSelect
            name={`namedConfigs[${index}].config.answers[${i}]`}
            key={vR.name}
            title={vR.name}
            value={
              cachedChoice
                ? {
                    label: cachedChoice.description,
                    value: cachedChoice.answer
                  }
                : ""
            }
            onChange={setFieldValue}
            options={vR.representations.map(o => {
              return {
                value: o.answer,
                label: o.description
              }
            })}
          />
        )
      })

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
            <NewSelect
              name={`namedConfigs[${index}].config.essenceFile`}
              value={values.namedConfigs[index].config.essenceFile}
              onChange={setFieldValue}
              title="Model"
              options={props.essenceFiles.map(file => {
                return { value: file, label: file }
              })}
            />

            <NewSelect
              name={`namedConfigs[${index}].config.paramFile`}
              value={values.namedConfigs[index].config.paramFile}
              onChange={setFieldValue}
              title="Param"
              options={props.paramFiles.map(file => {
                return { value: file, label: file }
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
              <>
                {!this.state.showReps[index] && (
                  <NewSelect
                    name={`namedConfigs[${index}].config.strategy`}
                    value={values.namedConfigs[index].config.strategy}
                    onChange={setFieldValue}
                    title="Strategy"
                    options={[
                      { value: "", label: "Default" },
                      { value: "c", label: "compact" },
                      { value: "s", label: "sparse" }
                    ]}
                  />
                )}
              </>
              <Check
                title={"Choose Representation"}
                checked={this.state.showReps[index]}
                onChange={() =>
                  this.setState((prevState: State) => {
                    let copy = cloneDeep(prevState.showReps)
                    copy[index] = !prevState.showReps[index]
                    return {
                      showReps: copy
                    }
                  })
                }
              />
              {this.state.showReps[index] && repSelectBoxes}
              {/*  */}
            </StageHeader>
            <StageHeader
              title="Savilerow"
              id={`sr${index + 1}`}
              isCollapsed={true}
            >
              <NewSelect
                name={`namedConfigs[${index}].config.optimisation`}
                value={values.namedConfigs[index].config.optimisation}
                onChange={setFieldValue}
                title="Optimisation"
                options={[
                  { value: "", label: "Default" },
                  { value: "-O0", label: "0" },
                  { value: "-O1", label: "1" },
                  { value: "-O2", label: "2" },
                  { value: "-O3", label: "3" }
                ]}
              />

              <NewSelect
                name={`namedConfigs[${index}].config.symmetry`}
                value={values.namedConfigs[index].config.symmetry}
                onChange={setFieldValue}
                title="Symmetry Breaking"
                options={[
                  { value: "", label: "Default" },
                  { value: "-S0", label: "0" },
                  { value: "-S1", label: "1" },
                  { value: "-S2", label: "2" }
                ]}
              />

              <NewSelect
                name={`namedConfigs[${index}].config.translation`}
                value={values.namedConfigs[index].config.translation}
                onChange={setFieldValue}
                title="Translation"
                options={[
                  { value: "", label: "Default" },
                  { value: "-no-cse", label: "No CSE" },
                  { value: "-identical-cse", label: "Identical CSE" },
                  { value: "-ac-cse", label: "AC CSE" },
                  { value: "-active-cse", label: "Active CSE" },
                  { value: "-active-ac-cse", label: "Active AC CSE" },
                  { value: "-deletevars", label: "Delete Vars" },
                  { value: "-reduce-domains", label: "Reduce Domains" },
                  {
                    value: "-reduce-domains-extend",
                    label: "Reduce Domains Extend"
                  },
                  { value: "-aggregate", label: "Aggregate" },
                  { value: "-tabulate", label: "Tabulate" },
                  { value: "-nomappers", label: "No Mappers" },
                  { value: "-minionmappers", label: "Minion Mappers" },
                  { value: "-no-bound-vars", label: "No Bound Variables" },
                  {
                    value: "-remove-redundant-vars",
                    label: "Remove Redundant Vars"
                  },
                  {
                    value: "-var-sym-breaking",
                    label: "Variable Symmetry Breaking"
                  }
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

              <NewSelect
                name={`namedConfigs[${index}].config.preprocessing`}
                value={values.namedConfigs[index].config.preprocessing}
                onChange={setFieldValue}
                title="Preprocessing"
                options={[
                  { value: "", label: "Default" },
                  { value: "GAC", label: "GAC" },
                  { value: "SACBounds", label: "SACBounds" },
                  { value: "SAC", label: "SAC" },
                  { value: "SSACBounds", label: "SSACBounds" },
                  { value: "SSAC", label: "SSAC" }
                ]}
              />
              <NewSelect
                name={`namedConfigs[${index}].config.consistency`}
                value={values.namedConfigs[index].config.consistency}
                onChange={setFieldValue}
                title="Consistency"
                options={[
                  { value: "", label: "Default" },
                  { value: "GAC", label: "GAC" },
                  { value: "SACBounds", label: "SACBounds" },
                  { value: "SAC", label: "SAC" },
                  { value: "SSACBounds", label: "SSACBounds" },
                  { value: "SSAC", label: "SSAC" }
                ]}
              />
            </StageHeader>
          </StageHeader>
        </div>
      )
    })
  }
  makeNamedConfig = (props: Props, index: number, reps: RepMap): Cache => {
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

    return {
      config: this.makeEmptyConfig(props, index, reps),
      name: name
    }
  }

  makeEmptyConfig = (props: Props, index: number, reps: RepMap): Config => {
    // console.log("selected cache args!", props.selectedCaches)

    const maxNumberOfQuestions =
      Object.values(reps).length > 0
        ? maxBy(Object.values(reps), x => x.length)!.length
        : 0

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
      preprocessing: "",
      answers: times(maxNumberOfQuestions, () => "")
    }

    if (!props.selectedCaches || !props.selectedCaches[index]) {
      return initialConfig
    }
    return this.overwriteWithCachedOptions(
      props.selectedCaches[index]!,
      initialConfig
    )
  }

  overwriteWithCachedOptions = (
    selectedCache: Cache,
    initialConfig: Config
  ): Config => {
    Object.keys(initialConfig).map(key => {
      if (key in selectedCache.config) {
        initialConfig[key] = selectedCache.config[key]
      }
    })

    initialConfig.answers = initialConfig.answers.map((str: string) => {
      return Number(str.split(":")[1])
    })

    console.log("selected Cache ", selectedCache)

    return initialConfig
  }

  componentDidUpdate(prevProps: Props) {
    if (this.props.selectedCaches) {
      if (!isEqual(this.props.selectedCaches, prevProps.selectedCaches)) {
        // console.log("HRE")

        let flag1 = false
        let flag2 = false
        // let config1Flag = false

        if (this.props.selectedCaches[0]) {
          if ("answers" in this.props.selectedCaches![0]!.config) {
            flag1 = true
          }
        }
        if (this.props.selectedCaches[1]) {
          if ("answers" in this.props.selectedCaches![1]!.config) {
            flag2 = true
          }
        }

        this.setState({ showReps: [flag1, flag2] })
      }
    }
  }

  render = () => {
    let list = this.props.diff
      ? [
          this.makeNamedConfig(this.props, 0, this.props.reps),
          this.makeNamedConfig(this.props, 1, this.props.reps)
        ]
      : [this.makeNamedConfig(this.props, 0, this.props.reps)]

    return (
      <Formik
        initialValues={{ namedConfigs: list }}
        onSubmit={values => {
          this.submissionHandler(values, this.props, this.state)
        }}
        validationSchema={validationSchema}
        enableReinitialize={true}
        render={({ values, setFieldValue }) => (
          <Form>
            <div className="row">
              <FieldArray
                name="configs"
                render={() =>
                  this.renderArrayElements(this.props, values, setFieldValue)
                }
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
}

export default ConfigForm
