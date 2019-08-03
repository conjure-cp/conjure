import * as Yup from "yup"
import { Cache, RepMap } from "../../../../extension/src/utils"
import { State, Values } from "./FormikConjure"
import { isEqual } from "lodash"

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

export const cleanCache = (cache: Cache, reps: RepMap, index: number) => {
  let cleaned = cache.config

  cleaned.answers = cache.config.answers.map(
    (answer: string | undefined, i: number) => {
      let variable = reps[cleaned.essenceFile][i]

      if (!answer) {
        return `${variable.name}:${1}`
      }

      return `${variable.name}:${answer}`
    }
  )

  let name =
    cache.name !== ""
      ? cache.name
      : `${new Date()
          .toLocaleTimeString()
          .replace(/ /g, "_")
          .replace(/,/g, "_")}_Config${index + 1}`

  if (cache.name.includes("_Config")) {
    name = cache.name.replace("_Config", `_Config${index + 1}`)
  }

  let newNamedConfig: Cache = {
    config: cleaned,
    name: name
  }

  return newNamedConfig
}
