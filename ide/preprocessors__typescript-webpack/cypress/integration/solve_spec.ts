import { vscodeServerBase } from '../support'

//test with no caches

describe('solving', () => {
	beforeEach('works', () => {
		cy.server() // enable response stubbing
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
