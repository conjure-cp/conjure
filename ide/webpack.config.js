'use strict'

const path = require('path')

module.exports = [
	// Webview ===============================

	{
		name: 'webview',
		entry: {
			main: './src/webclient/src/main.tsx',
			singleTreeVisualisation: './src/webclient/src/singleTreeVisualisation.tsx'
		},
		output: {
			path: path.resolve(__dirname, 'dist'),
			filename: '[name].js'
		},

		// Enable sourcemaps for debugging webpack's output.
		devtool: 'eval-source-map',
		// devtool: "cheap-module-source-map",

		resolve: {
			// Add '.ts' and '.tsx' as resolvable extensions.
			extensions: [ '.webpack.js', '.web.js', '.ts', '.tsx', '.js' ]
		},

		module: {
			rules: [
				{
					test: /\.tsx?$/,
					enforce: 'pre',
					loader: 'awesome-typescript-loader'
				},
				// All output '.js' files will have any sourcemaps re-processed by 'source-map-loader'.
				{ test: /\.js$/, enforce: 'pre', loader: 'source-map-loader' },
				{
					test: /\.css$/,
					use: [ 'style-loader', 'css-loader' ]
				}
			]
		},

		watchOptions: {
			aggregateTimeout: 300,
			poll: 1000,
			ignored: [ './node_modules/**', './preprocessors__typescript-webpack/**' ]
		}

		// Other options...
	},

	// VSCODE EXTENSION ====================================

	{
		name: 'extension',
		target: 'node', // vscode extensions run in a Node.js-context 📖 -> https://webpack.js.org/configuration/node/

		entry: './src/extension.ts', // the entry point of this extension, 📖 -> https://webpack.js.org/configuration/entry-context/
		output: {
			// the bundle is stored in the 'dist' folder (check package.json), 📖 -> https://webpack.js.org/configuration/output/
			path: path.resolve(__dirname, 'dist'),
			filename: 'extension.js',
			libraryTarget: 'commonjs2',
			devtoolModuleFilenameTemplate: '../[resource-path]'
		},
		devtool: 'source-map',
		externals: {
			vscode: 'commonjs vscode' // the vscode-module is created on-the-fly and must be excluded. Add other modules that cannot be webpack'ed, 📖 -> https://webpack.js.org/configuration/externals/
		},
		resolve: {
			// support reading TypeScript and JavaScript files, 📖 -> https://github.com/TypeStrong/ts-loader
			extensions: [ '.ts', '.js' ]
		},
		module: {
			rules: [
				{
					test: /\.ts$/,
					exclude: /node_modules/,
					use: [
						{
							loader: 'ts-loader'
						}
					]
				}
			]
		}
	}
]
