// import function "add" from another TypeScript file

describe('Checks selecting the caches box', () => {
	beforeEach('setup server', () => {
		cy.server() // enable response stubbing

		const serverBase = 'http://localhost:4000'

		cy.route('GET', `${serverBase}/config/files`, 'fixture:files.json')
		cy.route('GET', `${serverBase}/config/caches`, 'fixture:caches.json')
	})

	describe('Selecting a config file', () => {
		beforeEach('setup', () => {
			cy.visit('/')
		})

		it('Pick a config', () => {
			// cy.pause()
			cy.get('.css-1hwfws3').click()
			cy.get('.css-26l3qy-menu').click()

			cy.get('select[name="caches[0].essenceFile"]').should('have.value', 'model2.essence')
			cy.get('select[name="caches[0].paramFile"]').should('have.value', 'param2.param')
		})
	})
})
