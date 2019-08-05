import * as React from "react"
import { Caches } from "./Caches"
import * as ReactDOM from "react-dom"

import {
  Form,
  Field,
  FieldArray,
  FieldProps,
  Formik,
  FormikProps
} from "formik"
import * as Yup from "yup"

import { maxBy, times, isEqual, cloneDeep } from "lodash"
import TextWithLabel from "./TextWithLabel"
import SelectWithLabel from "./SelectWithLabel"
import StageHeader from "../common/StageHeader"
import Checkbox from "./Checkbox"
import { Check } from "../common/Check"
import { defaultProps } from "react-select/lib/Creatable"

interface Props {
  index: number
}

interface MinionConfig {
  nodeLimit: number
  solLimit: number
  minionTime: number
  preprocessing: number
  consistency: number
  minionSwitches: []
}

interface Values {
  config: MinionConfig
}



export default TextWithLabel

export const MinionStage = (
  props: Props & FormikProps<Values> & FieldProps<any>
) => {
  // export class MinionStage extends React.Component<Props, any> {
  // render() {
  const { index, values } = props
  const { name } = props.field

  let consistencyOptions = [
    { value: "", label: "Default" },
    { value: "GAC", label: "GAC" },
    { value: "SACBounds", label: "SACBounds" },
    { value: "SAC", label: "SAC" },
    { value: "SSACBounds", label: "SSACBounds" },
    { value: "SSAC", label: "SSAC" }
  ]

  // console.log("IN MINION ", props.values)

  return (
    <StageHeader title="Minion" id={`minion${index + 1}`} isCollapsed={true}>
      <Checkbox
        name={`${name}.minionSwitches`}
        value="-findallsols"
        label="Find all solutions"
        values={values.config.minionSwitches}
      />
      <Checkbox
        name={`${name}.minionSwitches`}
        value="-randomiseorder"
        label="Randomise Var Order"
        values={values.config.minionSwitches}
      />
      <Field
        name={`${name}.nodeLimit`}
        component={TextWithLabel}
        values={values.config.nodeLimit}
        label="Node Limit"
      />
      <Field
        name={`${name}.solLimit`}
        component={TextWithLabel}
        values={values.config.solLimit}
        label="Solution Limit"
      />
      <Field
        name={`${name}.minionTime`}
        component={TextWithLabel}
        values={values.config.minionTime}
        label="CPU Limit"
      />

      <Field
        name={`${name}.preprocessing`}
        component={SelectWithLabel}
        title="preprocessing"
        values={values.config.preprocessing}
        options={consistencyOptions}
      />
      <Field
        name={`${name}.consistency`}
        component={SelectWithLabel}
        title="Consistency"
        values={values.config.consistency}
        options={consistencyOptions}
      />
    </StageHeader>
  )
}

// export const getSRStage = (index: number) => {
//   return (
//     <StageHeader title="Savilerow" id={`sr${index + 1}`} isCollapsed={true}>
//       <Field
//         name={`namedCaches[${index}].config.optimisation`}
//         component={SelectWithLabel}
//         title="Optimisation"
//         options={[
//           { value: "", label: "Default" },
//           { value: "-O0", label: "0" },
//           { value: "-O1", label: "1" },
//           { value: "-O2", label: "2" },
//           { value: "-O3", label: "3" }
//         ]}
//       />

//       <Field
//         name={`namedCaches[${index}].config.symmetry`}
//         component={SelectWithLabel}
//         title="Symmetry Breaking"
//         options={[
//           { value: "", label: "Default" },
//           { value: "-S0", label: "0" },
//           { value: "-S1", label: "1" },
//           { value: "-S2", label: "2" }
//         ]}
//       />

//       <Field
//         name={`namedCaches[${index}].config.translation`}
//         component={SelectWithLabel}
//         title="Translation"
//         options={[
//           { value: "", label: "Default" },
//           { value: "-no-cse", label: "No CSE" },
//           { value: "-identical-cse", label: "Identical CSE" },
//           { value: "-ac-cse", label: "AC CSE" },
//           { value: "-active-cse", label: "Active CSE" },
//           { value: "-active-ac-cse", label: "Active AC CSE" },
//           { value: "-deletevars", label: "Delete Vars" },
//           { value: "-reduce-domains", label: "Reduce Domains" },
//           {
//             value: "-reduce-domains-extend",
//             label: "Reduce Domains Extend"
//           },
//           { value: "-aggregate", label: "Aggregate" },
//           { value: "-tabulate", label: "Tabulate" },
//           { value: "-nomappers", label: "No Mappers" },
//           { value: "-minionmappers", label: "Minion Mappers" },
//           { value: "-no-bound-vars", label: "No Bound Variables" },
//           {
//             value: "-remove-redundant-vars",
//             label: "Remove Redundant Vars"
//           },
//           {
//             value: "-var-sym-breaking",
//             label: "Variable Symmetry Breaking"
//           }
//         ]}
//       />
//       <Field
//         name={`namedCaches[${index}].config.srTime`}
//         component={TextWithLabel}
//         label="Time limit"
//       />
//       <Field
//         name={`namedCaches[${index}].config.cnfLimit`}
//         component={TextWithLabel}
//         label="CNF clause limit"
//       />
//     </StageHeader>
//   )
// }

// export const getConjureStage = (index: number, showReps: boolean[]) => {
//   return (

//   <StageHeader
//               title="Conjure"
//               id={`conjure${index + 1}`}
//               isCollapsed={true}
//             >
//               <Field
//                 name={`namedCaches[${index}].config.conjureTime`}
//                 component={TextWithLabel}
//                 label={"Time limit"}
//               />
//               <>
//                 {!showReps[index] && (
//                   <Field
//                     name={`namedCaches[${index}].config.strategy`}
//                     component={SelectWithLabel}
//                     title="Strategy"
//                     options={[
//                       { value: "", label: "Default" },
//                       { value: "c", label: "compact" },
//                       { value: "s", label: "sparse" }
//                     ]}
//                   />
//                 )}
//               </>
//               <Check
//                 title={"Choose Representation"}
//                 checked={showReps[index]}
//                 onChange={() =>
//                   this.setState((prevState: State) => {
//                     let copy = cloneDeep(prevState.showReps)
//                     copy[index] = !prevState.showReps[index]
//                     return {
//                       showReps: copy
//                     }
//                   })
//                 }
//               />
//               {/* {this.state.showReps[index] && repSelectBoxes} */}
//               {/*  */}
//             </StageHeader>
//   )
