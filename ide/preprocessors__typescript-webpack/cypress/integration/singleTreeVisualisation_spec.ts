import { vscodeServerBase } from '../support'

//test with no caches

describe('tree vis', () => {
	beforeEach('setup', () => {
		cy.server() // enable response stubbing
		cy.route('GET', `${vscodeServerBase}/test/tree`, 'fixture:solve.json')
		cy.visit('/singleTreeVisualisation.html')
	})

	it('Checks can select answers', () => {
		cy.get('.loadedContent > .btn').click()
	})
})
