// ***********************************************************
// This example support/index.js is processed and
// loaded automatically before your test files.
//
// This is a great place to put global configuration and
// behavior that modifies Cypress.
//
// You can change the location of this file or turn off
// automatically serving support files with the
// 'supportFile' configuration option.
//
// You can read more here:
// https://on.cypress.io/configuration
// ***********************************************************

// Import commands.js using ES2015 syntax:
import './commands'
// import 'cypress-react-unit-test'
require('cypress-skip-and-only-ui/support')

Cypress.on('window:before:load', (win) => {
	delete win.fetch
})
export const vscodeServerBase = 'http://localhost:4000'
export const delay = 100

// Alternatively you can use CommonJS syntax:
// require('./commands')
