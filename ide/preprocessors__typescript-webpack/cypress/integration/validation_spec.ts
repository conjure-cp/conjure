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

	it('no files endpoint', () => {
		cy.server()
		cy.route('GET', `${vscodeServerBase}/config/caches`, 'fixture:noCaches.json')
		cy.visit('/')
		cy.contains('config/files')
	})

	it('no caches endpoint', () => {
		cy.server()
		cy.route('GET', `${vscodeServerBase}/config/files`, 'fixture:weirdFileNames.json')
		cy.visit('/')
		cy.contains('config/caches')
	})
})

describe('nimServerProblems', () => {
	beforeEach('setup', () => {
		cy.server()
		cy.route('GET', `${vscodeServerBase}/config/caches`, 'fixture:caches.json')
		cy.route('GET', `${vscodeServerBase}/config/files`, 'fixture:files.json')
		cy.route('POST', `${vscodeServerBase}/config/solve`, 'fixture:normal-8/initialResponseWrongNimPort.json')
		cy.visit('/')
		cy.get('form > .loadedContent > .btn').click()
	})

	it.only('shows error message if cannot make request to nim server', () => {
		cy.wait(1000)

		for (let i = 0; i < 4; i++) {
			cy.get('#treeSVG').type('s', { delay: 500 })
		}

		cy.contains('ERROR')
		cy.contains('nim')
	})
})
