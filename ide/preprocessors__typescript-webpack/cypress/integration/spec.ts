// import function "add" from another TypeScript file

describe('Homepage', () => {
	beforeEach('works', () => {
		cy.server() // enable response stubbing

		const serverBase = 'http://localhost:4000'

		cy.route('GET', `${serverBase}/config/files`, 'fixture:files.json')
		cy.route('GET', `${serverBase}/config/caches`, 'fixture:caches.json')
	})

	describe('Conjure', () => {
		beforeEach('expand', () => {
			cy.visit('/')
			cy.contains('Conjure').click()
		})

		it('Checks invalid field in conjure', () => {
			cy.get('input[name="caches[0].config.conjureConfig.conjureTime"]').type('dasas')
			cy.contains('Leave empty or specify an integer > 0')
			cy.contains('Fix the errors first!')
		})

		it('Checks can select answers', () => {
			cy.get('input[id="ChooseRepresentation-check"]').click()
			cy.contains('x')
			cy.contains('explicit')
		})
	})

	describe('Savile Row', () => {
		beforeEach('expand', () => {
			cy.visit('/')
			cy.contains('Savile Row').click()
		})

		it('Checks time field in SR', () => {
			cy.get('input[name="caches[0].config.srConfig.srTime"]').type('blah1')
			cy.contains('Leave empty or specify an integer > 0')
			cy.contains('Fix the errors first!')
		})

		it('Checks clause field in SR', () => {
			cy.get('input[name="caches[0].config.srConfig.cnfLimit"]').type('dasas')
			cy.contains('Leave empty or specify an integer > 0')
			cy.contains('Fix the errors first!')
		})
	})

	describe('Minion', () => {
		beforeEach('expand', () => {
			cy.visit('/')
			// Need to specify a otheriws it will pick up on the minionmappers option in the SR translation select box.
			cy.contains('a', 'Minion').click()
		})

		it('Checks nodelimit field in Minion', () => {
			cy.get('input[name="caches[0].config.minionConfig.nodeLimit"]').type('dasas')
			cy.contains('Leave empty or specify an integer > 0')
			cy.contains('Fix the errors first!')
		})

		it('Checks sollimit field in Minion', () => {
			cy.get('input[name="caches[0].config.minionConfig.solLimit"]').type('dasas')
			cy.contains('Leave empty or specify an integer > 0')
			cy.contains('Fix the errors first!')
		})

		it('Checks minionTimelimit field in Minion', () => {
			cy.get('input[name="caches[0].config.minionConfig.minionTime"]').type('dasas')
			cy.contains('Leave empty or specify an integer > 0')
			cy.contains('Fix the errors first!')
		})
	})
})
