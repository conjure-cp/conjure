import * as Yup from "yup"
import { RepMap } from "../../../../extension/src/configHelper"
import { Cache } from "../../../../extension/src/utils";
import { State, Values } from "./FormikConjure";
import { isEqual } from "lodash";

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
  solLimit: intOrNothing
}

const namedConfigSchema = {
  config: Yup.object().shape(configSchema),
  name: Yup.string().matches(
    /^$|^[a-z_:0-9]+$/i,
    "Name nust be alphanumeric(underscore/colons allowed), leave blank for timestamp"
  )
}

export const validationSchema = Yup.object().shape({
  namedCaches: Yup.array().of(Yup.object().shape(namedConfigSchema))
})



export const cleanCache = (cache: Cache, state: State, values: Values, reps: RepMap,  index: number) => {
    const config = cache.config

    let cleaned: any = {}

    Object.keys(config).map((key: string) => {
      if (config[key] !== "" && config[key] !== "Default") {
        cleaned[key] = config[key]
      }
    })

    if (config.minionSwitches) {
      cleaned["minionSwitches"] = config.minionSwitches
    }

    // console.log(config.answers)

    cleaned.answers = config.answers.map(
      (answer: number | string, i: number) => {
        let variable = reps[cleaned.essenceFile][i]

        if (!answer) {
          return `${variable.name}:${1}`
        }

        return `${variable.name}:${answer}`
      }
    )

    if (!state.showReps[index]) {
      delete cleaned["answers"]
    }

    let name =
      cache.name !== ""
        ? cache.name
        : `${new Date()
            .toLocaleTimeString()
            .replace(/ /g, "_")
            .replace(/,/g, "_")}_Config`

    if (isEqual(values.namedCaches[0], values.namedCaches[1])) {
      name += "1+2"
    } else {
      name += `${index + 1}`
    }

    let newNamedConfig: Cache = {
      config: cleaned,
      name: name
    }

    return newNamedConfig
  }