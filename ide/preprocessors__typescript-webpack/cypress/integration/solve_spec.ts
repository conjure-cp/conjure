import { vscodeServerBase } from '../support'

//test with no caches

describe('solving', () => {
	describe('Solve ok', () => {
		beforeEach('expand', () => {
			cy.server({ delay: 1500 }) // enable response stubbing
			cy.route('GET', `${vscodeServerBase}/config/files`, 'fixture:files.json')
			cy.route('GET', `${vscodeServerBase}/config/caches`, 'fixture:caches.json')
			cy.route('POST', `${vscodeServerBase}/config/solve`, 'fixture:solve.json')
			cy.visit('/')
		})

		it('Checks can select answers', () => {
			cy.get('.loadedContent > .btn').click()
		})
	})

	describe('server error json', () => {
		it('Checks can show error message', () => {
			cy.server() // enable response stubbing
			cy.route('GET', `${vscodeServerBase}/config/files`, 'fixture:files.json')
			cy.route('GET', `${vscodeServerBase}/config/caches`, 'fixture:caches.json')
			cy.route('POST', `${vscodeServerBase}/config/solve`, 'fixture:serverError.json')
			cy.visit('/')
			cy.get('.loadedContent > .btn').click()
			cy.contains('SERVER ERROR')
		})
	})
	describe('server error text', () => {
		it('Checks can show error message', () => {
			cy.server() // enable response stubbing
			cy.route('GET', `${vscodeServerBase}/config/files`, 'fixture:files.json')
			cy.route('GET', `${vscodeServerBase}/config/caches`, 'fixture:caches.json')
			cy.route('POST', `${vscodeServerBase}/config/solve`, 'fixture:notEvenJson.txt')
			cy.visit('/')
			cy.get('.loadedContent > .btn').click()
			cy.contains('SERVER ERROR')
		})
	})
})

// TODO no response from the server, It will just keep waiting -> okay I guess
