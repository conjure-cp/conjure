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

export const validationSchema = Yup.object().shape({
  caches: Yup.array().of(
    Yup.object().shape({
      name: Yup.string().matches(
        /^$|^[a-z_:0-9]+$/i,
        "Name nust be alphanumeric(underscore/colons allowed), leave blank for timestamp"
      ),
      config: Yup.object().shape({
        conjureConfig: Yup.object().shape({
          conjureTime: intOrNothing
        }),
        srConfig: Yup.object().shape({
          srTime: intOrNothing,
          cnfLimit: intOrNothing
        }),
        minionConfig: Yup.object().shape({
          minionTime: intOrNothing,
          solLimit: intOrNothing,
          nodeLimit: intOrNothing
        })
      })
    })
  )
})


export const cleanCache = (cache: Cache, index: number) => {
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
    ...cache,
    name
  }

  return newNamedConfig
}
