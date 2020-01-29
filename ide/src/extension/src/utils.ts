import * as path from 'path'
import { CombinedConfig } from '../../webclient/src/components/config/ConfigArrayElement'
import { min } from 'd3'

export type RepMap = Record<string, VarRepresentation[]>

export interface VarRepresentation {
	name: string
	representations: RepOption[]
}

export interface RepOption {
	answer: string
	description: string
}

export interface ToProcess {
	args: string[]
	hash: string
	config: any
	name: string
}
export interface Separation {
	needToGenerate: ToProcess[]
	loadFromCache: ToProcess[]
}
export interface Cache {
	name: string
	essenceFile: string
	paramFile: string
	config: CombinedConfig
}

export const newCache = (): Cache => {
	return {
		name: '',
		essenceFile: '',
		paramFile: '',
		config: {
			conjureConfig: { conjureTime: '', strategy: '', answers: [] },
			srConfig: {
				optimisation: '',
				symmetry: '',
				translation: '',
				srTime: '',
				cnfLimit: '',
			},
			minionConfig: {
				nodeLimit: '',
				solLimit: '',
				minionTime: '',
				preprocessing: '',
				consistency: '',
				minionSwitches: [],
			},
		},
	}
}

export const cacheToArgs = (cache: Cache, cacheFolderPath: string, hash: string): string[] => {
	const config = cache.config
	const { conjureConfig, srConfig, minionConfig } = config

	const outputPath = path.join(cacheFolderPath, hash)

	const fullPathToModel = path.join(path.dirname(cacheFolderPath), cache.essenceFile)
	const fullPathToParam = path.join(path.dirname(cacheFolderPath), cache.paramFile)

	let conjureOptions = [ 'solve', fullPathToModel, fullPathToParam, '-o', outputPath ]

	if (conjureConfig.conjureTime !== '') {
		conjureOptions.push(`--limit-time=${conjureConfig.conjureTime}`)
	}

	if (conjureConfig.strategy !== '') {
		conjureOptions.push('-a')
		conjureOptions.push(conjureConfig.strategy)
	}

	if (!conjureConfig.answers.find((x) => x === undefined) && conjureConfig.answers.length > 0) {
		conjureOptions.push('-aai')
		conjureOptions.push('--channelling=no')
		conjureOptions.push('--smart-filenames')
		conjureOptions.push('--responses')
		conjureOptions.push(
			`${conjureConfig.answers
				.map((x) => {
					if (!x) {
						return 1
					}
					return x
				})
				.join(',')
				.replace(/(^,)|(,$)/g, '')}`,
		)
	}

	let savileRowOptions = [ '--savilerow-options', '"' ]

	if (srConfig.optimisation !== '') {
		savileRowOptions.push(srConfig.optimisation)
	}

	if (srConfig.symmetry !== '') {
		savileRowOptions.push(srConfig.symmetry)
	}

	if (srConfig.translation !== '') {
		savileRowOptions.push(srConfig.translation)
	}

	if (srConfig.srTime !== '') {
		savileRowOptions.push('-timelimit')
		savileRowOptions.push(String(srConfig.srTime))
	}

	if (srConfig.cnfLimit !== '') {
		savileRowOptions.push('-cnflimit')
		savileRowOptions.push(String(srConfig.cnfLimit))
	}

	savileRowOptions.push('"')

	let minionOptions = [ '--solver-options', '"-dumptreesql', path.join(outputPath, '/out.db') ]

	minionOptions = minionOptions.concat(minionConfig.minionSwitches)

	if (minionConfig.nodeLimit !== '') {
		minionOptions.push('-nodelimit')
		minionOptions.push(String(minionConfig.nodeLimit))
	}

	if (minionConfig.solLimit !== '') {
		minionOptions.push('-sollimit')
		minionOptions.push(String(minionConfig.solLimit))
	}

	if (minionConfig.minionTime !== '') {
		minionOptions.push('-cpulimit')
		minionOptions.push(String(minionConfig.minionTime))
	}

	if (minionConfig.preprocessing !== '') {
		minionOptions.push('-preprocess')
		minionOptions.push(minionConfig.preprocessing)
	}

	if (minionConfig.consistency !== '') {
		minionOptions.push('-prop-node')
		minionOptions.push(minionConfig.consistency)
	}

	minionOptions.push('"')

	conjureOptions = conjureOptions.concat(savileRowOptions).concat(minionOptions)

	return conjureOptions
}
