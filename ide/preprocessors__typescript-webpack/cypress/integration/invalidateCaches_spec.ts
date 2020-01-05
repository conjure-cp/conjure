import { vscodeServerBase } from '../support'

describe('Homepage', () => {
	beforeEach('works', () => {
		cy.server({ delay: 500 }) // enable response stubbing
		cy.route('GET', `${vscodeServerBase}/config/files`, 'fixture:files.json')
		cy.route('GET', `${vscodeServerBase}/config/caches`, 'fixture:caches.json')
		cy.visit('/')
	})

	it('shows error message when request failes', () => {
		cy.wait(1000)
		cy.contains('nval').click()
		cy.contains('ERROR')
		cy.contains('config/invalidateCaches')
	})

	it('is able to make a request to invalidate caches', () => {
		cy.route('GET', `${vscodeServerBase}/config/invalidateCaches`, {})
		cy.wait(1000)
		cy.contains('nval').click()
	})
})
