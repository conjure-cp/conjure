import { vscodeServerBase } from '../support'

describe('propsValidationTests', () => {
	it('shows an error messsage if there are no essence files', () => {
		cy.server()
		cy.route('GET', `${vscodeServerBase}/config/files`, 'fixture:noEssenceFiles.json')
		cy.route('GET', `${vscodeServerBase}/config/caches`, 'fixture:caches.json')
		cy.visit('/')
		cy.contains('No essence files detected!')
	})

	it('shows an error messsage if there are no param files', () => {
		cy.server()
		cy.route('GET', `${vscodeServerBase}/config/files`, 'fixture:noParamFiles.json')
		cy.route('GET', `${vscodeServerBase}/config/caches`, 'fixture:caches.json')
		cy.visit('/')
		cy.contains('No param files detected!')
	})

	it('shows an error messsage if the caches are invalid', () => {
		cy.server()
		cy.route('GET', `${vscodeServerBase}/config/files`, 'fixture:weirdFileNames.json')
		cy.route('GET', `${vscodeServerBase}/config/caches`, 'fixture:caches.json')
		cy.visit('/')
		cy.contains('There exists a cache referencing an essence file that does not exist.')
	})

	it('shows an error messsage if the caches are invalid', () => {
		cy.server()
		cy.route('GET', `${vscodeServerBase}/config/files`, 'fixture:weirdFileNames.json')
		cy.route('GET', `${vscodeServerBase}/config/caches`, 'fixture:caches.json')
		cy.visit('/')
		cy.contains('There exists a cache referencing an essence file that does not exist.')
	})

	it('no caches', () => {
		cy.server()
		cy.route('GET', `${vscodeServerBase}/config/files`, 'fixture:weirdFileNames.json')
		cy.route('GET', `${vscodeServerBase}/config/caches`, 'fixture:noCaches.json')
		cy.visit('/')
		cy.contains('There exists an essenceFile without a corresponding "reps" field.')
	})
})
