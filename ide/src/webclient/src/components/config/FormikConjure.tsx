import * as React from "react"
import { Caches } from "./Caches"
import * as ReactDOM from "react-dom"

import { Form, Field, FieldArray, Formik, FormikProps } from "formik"
import * as Yup from "yup"

import { maxBy, times, isEqual, cloneDeep } from "lodash"
import TextWithLabel from "./TextWithLabel"
import SelectWithLabel from "./SelectWithLabel"
import StageHeader from "../common/StageHeader"
import Checkbox from "./Checkbox"
import { Check } from "../common/Check"
import NewSelect from "./NewSelect"
import { Cache, RepMap } from "../../../../extension/src/utils"
import { validationSchema, cleanCache } from "./Validation"

type RepChoices = Record<string, string>

export interface Values {
  namedCaches: Cache[]
}

export interface Config {
  answers: (string | undefined)[]
  essenceFile: string
  paramFile: string
  strategy: string
  optimisation: string
  symmetry: string
  translation: string
  minionSwitches: string[]
  nodeLimit: number | string
  solLimit: number | string
  conjureTime: number | string
  srTime: number | string
  minionTime: number | string
  cnfLimit: number | string
  consistency: string
  preprocessing: string
  [key: string]: any
}

export const newCache = (): Cache => {
  return {
    name: "",
    config: newConfig()
  }
}

export const newConfig = (): Config => {
  return {
    paramFile: "",
    essenceFile: "",
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
    solLimit: "",
    consistency: "",
    preprocessing: "",
    answers: []
  }
}

export interface Props {
  vscodeServerPort: number
  diff: boolean
  caches: Cache[]
  cacheChangeHandler: (cache: Cache, index: number) => void
  essenceFiles: string[]
  paramFiles: string[]
  reps: RepMap
  selectedCaches?: (Cache | undefined)[]
  responseHandler: (content: any) => void
  diffCheckHandler: (namedCache1: Cache) => void
}

export interface State {
  showReps: boolean[]
}

class ConfigForm extends React.Component<Props, State> {
  constructor(props: Props) {
    super(props)
    this.state = { showReps: [false, false] }
  }

  submissionHandler = (values: Values, props: Props) => {
    let cleaned = values.namedCaches.map((namedCache, index) =>
      cleanCache(namedCache, props.reps, index)
    )

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
    // console.log(values)

    return values.namedCaches.map((cache, index) => {
      const currentEssenceFile = values.namedCaches[index].config.essenceFile

      const varReps = currentEssenceFile ? props.reps[currentEssenceFile] : []

      // const repSelectBoxes = varReps.map((vR, i) => {
      // values.namedCaches[index].config.answers[i] =
      //   values.namedCaches[index].config.answers[i] === ""
      //     ? vR.representations[0].answer
      //     : values.namedCaches[index].config.answers[i]

      // const cachedChoice = vR.representations.find(
      //   x => x.answer === values.namedCaches[index].config.answers[i]
      // )

      // let cachedChoice = cache.config.answers[i]

      // return (
      //   <NewSelect
      //     name={`namedCaches[${index}].config.answers[${i}]`}
      //     key={vR.name}
      //     title={vR.name}
      //     value={
      //       cachedChoice
      //         ? {
      //             label: cachedChoice.description,
      //             value: cachedChoice.answer
      //           }
      //         : ""
      //     }
      //     onChange={setFieldValue}
      //     options={vR.representations.map(o => {
      //       return {
      //         value: o.answer,
      //         label: o.description
      //       }
      //     })}
      //   />
      // )
      // })

      return (
        <div className="col" key={index}>
          <StageHeader
            isCollapsed={false}
            title={`Configuration ${index + 1}`}
            id={`config${index + 1}`}
          >
            <Field
              name={`namedCaches[${index}].name`}
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
              name={`namedCaches[${index}].config.essenceFile`}
              component={SelectWithLabel}
              title="Model"
              options={props.essenceFiles.map(file => {
                return { value: file, label: file }
              })}
            />

            <Field
              name={`namedCaches[${index}].config.paramFile`}
              component={SelectWithLabel}
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
                name={`namedCaches[${index}].config.conjureTime`}
                component={TextWithLabel}
                label={"Time limit"}
              />
              <>
                {!this.state.showReps[index] && (
                  <Field
                    name={`namedCaches[${index}].config.strategy`}
                    component={SelectWithLabel}
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
              {/* {this.state.showReps[index] && repSelectBoxes} */}
              {/*  */}
            </StageHeader>
            <StageHeader
              title="Savilerow"
              id={`sr${index + 1}`}
              isCollapsed={true}
            >
              <Field
                name={`namedCaches[${index}].config.optimisation`}
                component={SelectWithLabel}
                title="Optimisation"
                options={[
                  { value: "", label: "Default" },
                  { value: "-O0", label: "0" },
                  { value: "-O1", label: "1" },
                  { value: "-O2", label: "2" },
                  { value: "-O3", label: "3" }
                ]}
              />

              <Field
                name={`namedCaches[${index}].config.symmetry`}
                component={SelectWithLabel}
                title="Symmetry Breaking"
                options={[
                  { value: "", label: "Default" },
                  { value: "-S0", label: "0" },
                  { value: "-S1", label: "1" },
                  { value: "-S2", label: "2" }
                ]}
              />

              <Field
                name={`namedCaches[${index}].config.translation`}
                component={SelectWithLabel}
                value={values.namedCaches[index].config.translation}
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
                name={`namedCaches[${index}].config.srTime`}
                component={TextWithLabel}
                label="Time limit"
              />
              <Field
                name={`namedCaches[${index}].config.cnfLimit`}
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
                name={`namedCaches[${index}].config.minionSwitches`}
                value="-findallsols"
                label="Find all solutions"
              />
              <Checkbox
                name={`namedCaches[${index}].config.minionSwitches`}
                value="-randomiseorder"
                label="Randomise Var Order"
              />
              <Field
                name={`namedCaches[${index}].config.nodeLimit`}
                component={TextWithLabel}
                label="Node Limit"
              />
              <Field
                name={`namedCaches[${index}].config.solLimit`}
                component={TextWithLabel}
                label="Solution Limit"
              />
              <Field
                name={`namedCaches[${index}].config.minionTime`}
                component={TextWithLabel}
                label="CPU Limit"
              />

              <Field
                name={`namedCaches[${index}].config.preprocessing`}
                component={SelectWithLabel}
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
              <Field
                name={`namedCaches[${index}].config.consistency`}
                component={SelectWithLabel}
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
      solLimit: "",
      consistency: "",
      preprocessing: "",
      answers: times(maxNumberOfQuestions, () => undefined)
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

    if (selectedCache.config.answers) {
      initialConfig.answers = initialConfig.answers.map((str: any) => {
        return str.split(":")[1]
      })
    }

    // console.log("selected Cache ", selectedCache)

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
        initialValues={{ namedCaches: list }}
        onSubmit={values => {
          this.submissionHandler(values, this.props)
        }}
        validationSchema={validationSchema}
        enableReinitialize={true}
        render={({ values, setFieldValue }) => (
          <Form>
            <Check
              title={"Compare trees"}
              checked={this.props.diff}
              onChange={() => {
                this.props.diffCheckHandler(
                  cleanCache(values.namedCaches[0], this.props.reps, 0)
                )
              }}
            />
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
