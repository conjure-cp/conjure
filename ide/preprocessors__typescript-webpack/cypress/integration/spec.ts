// import function "add" from another TypeScript file

describe('Homepage', () => {
	before('works', () => {
		cy.server() // enable response stubbing

		const serverBase = 'http://localhost:4000'

		cy.route('GET', `${serverBase}/config/files`, 'fixture:files.json')
		cy.route('GET', `${serverBase}/config/caches`, 'fixture:caches.json')

		cy.visit('/')
	})

	it('works', () => {})
})
