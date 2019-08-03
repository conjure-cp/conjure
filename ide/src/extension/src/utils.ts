import { Config } from "../../webclient/src/components/config/FormikConjure";

import * as path from "path"


export type RepMap = Record<string, VarRepresentation[]>

export interface VarRepresentation {
  name: string;
  representations: RepOption[];
}

export interface RepOption {
  answer: number;
  description: string;
}

export interface ToProcess {
  args: string[];
  hash: string;
  config: any;
  name: string;
}
export interface Separation {
  needToGenerate: ToProcess[];
  loadFromCache: ToProcess[];
}
export interface Cache {
  name: string;
  config: Config;
}


export const cacheToArgs = (
  cache: Cache,
  cacheFolderPath: string
): string[] => {
  const config = cache.config

  const outputPath = path.join(cacheFolderPath, cache.name)

  const fullPathToModel = path.join(
    path.dirname(cacheFolderPath),
    config.essenceFile
  )
  const fullPathToParam = path.join(
    path.dirname(cacheFolderPath),
    config.paramFile
  )

  let conjureOptions = [
    "solve",
    fullPathToModel,
    fullPathToParam,
    "-o",
    outputPath
  ]

  if (config.conjureTime !== "") {
    conjureOptions.push(`--limit-time=${config.conjureTime}`)
  }

  if (config.strategy !== "") {
    conjureOptions.push("-a")
    conjureOptions.push(config.strategy)
  }

  if (!config.answers.find(x => x === undefined) && config.answers.length > 0) {
    conjureOptions.push("-aai")
    conjureOptions.push("--channelling=no")
    conjureOptions.push("--smart-filenames")
    conjureOptions.push("--responses-representation")
    conjureOptions.push(`${config.answers.join(",")}`)
  }

  let savileRowOptions = ["--savilerow-options", '"']

  if (config.optimisation !== "") {
    savileRowOptions.push(config.optimisation)
  }

  if (config.symmetry !== "") {
    savileRowOptions.push(config.symmetry)
  }

  if (config.translation !== "") {
    savileRowOptions.push(config.translation)
  }

  if (config.srTime !== "") {
    savileRowOptions.push("-timelimit")
    savileRowOptions.push(String(config.srTime))
  }

  if (config.cnfLimit !== "") {
    savileRowOptions.push("-cnflimit")
    savileRowOptions.push(String(config.cnfLimit))
  }

  savileRowOptions.push('"')

  let minionOptions = [
    "--solver-options",
    '"-dumptreesql',
    path.join(outputPath, "/out.db")
  ]

  minionOptions = minionOptions.concat(config.minionSwitches)

  if (config.nodeLimit !== "") {
    minionOptions.push("-nodelimit")
    minionOptions.push(String(config.nodeLimit))
  }

  if (config.solLimit !== "") {
    minionOptions.push("-sollimit")
    minionOptions.push(String(config.solLimit))
  }

  if (config.minionTime !== "") {
    minionOptions.push("-cpulimit")
    minionOptions.push(String(config.minionTime))
  }

  if (config.preprocessing !== "") {
    minionOptions.push("-preprocess")
    minionOptions.push(config.preprocessing)
  }

  if (config.consistency !== "") {
    minionOptions.push("-prop-node")
    minionOptions.push(config.consistency)
  }

  minionOptions.push('"')

  conjureOptions = conjureOptions.concat(savileRowOptions).concat(minionOptions)

  return conjureOptions
}