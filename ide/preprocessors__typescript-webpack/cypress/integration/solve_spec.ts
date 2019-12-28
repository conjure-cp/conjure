import { vscodeServerBase } from '../support'

//test with no caches

describe('solving', () => {
	beforeEach('works', () => {
		cy.server({ delay: 1500 }) // enable response stubbing
		cy.route('GET', `${vscodeServerBase}/config/files`, 'fixture:files.json')
		cy.route('GET', `${vscodeServerBase}/config/caches`, 'fixture:caches.json')
		cy.route('POST', `${vscodeServerBase}/config/solve`, 'fixture:solve.json')
	})

	describe('Solve', () => {
		beforeEach('expand', () => {
			cy.visit('/')
		})

		it('Checks can select answers', () => {
			cy.get('.loadedContent > .btn').click()
		})
	})
})

// TODO no response from the server, It will just keep waiting -> okay I guess
// TODO response from the server is an error message, still JSON tho
