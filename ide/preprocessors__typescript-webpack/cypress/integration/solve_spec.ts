import { vscodeServerBase } from '../support'

//test with no caches

describe('solving', () => {
	describe('server error json', () => {
		it('Checks can show error message', () => {
			cy.server() // enable response stubbing
			cy.route('GET', `${vscodeServerBase}/config/files`, 'fixture:files.json')
			cy.route('GET', `${vscodeServerBase}/config/caches`, 'fixture:caches.json')
			cy.route('POST', `${vscodeServerBase}/config/solve`, 'fixture:serverError.json')
			cy.visit('/')
			cy.get('form > .loadedContent > .btn').click()
			cy.contains('ERROR')
		})
	})
	describe('server error text', () => {
		it('Checks can show error message', () => {
			cy.server() // enable response stubbing
			cy.route('GET', `${vscodeServerBase}/config/files`, 'fixture:files.json')
			cy.route('GET', `${vscodeServerBase}/config/caches`, 'fixture:caches.json')
			cy.route('POST', `${vscodeServerBase}/config/solve`, 'fixture:notEvenJson.txt')
			cy.visit('/')
			cy.get('form > .loadedContent > .btn').click()
			cy.contains('ERROR')
		})
	})

	describe('its ok', () => {
		it('Checks can solve', () => {
			cy.server({ delay: 1500 }) // enable response stubbing
			cy.route('GET', `${vscodeServerBase}/config/files`, 'fixture:files.json')
			cy.route('GET', `${vscodeServerBase}/config/caches`, 'fixture:caches.json')
			cy.route('POST', `${vscodeServerBase}/config/solve`, 'fixture:normal-8/initialResponse.json')
			cy.visit('/')

			cy.get('.loadedContent > .btn').click()
		})
	})
})

// TODO no response from the server, It will just keep waiting -> okay I guess
