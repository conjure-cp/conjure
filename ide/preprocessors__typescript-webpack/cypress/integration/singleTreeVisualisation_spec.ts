import { vscodeServerBase } from '../support'

//test with no caches

describe('tree vis', () => {
	beforeEach('setup', () => {
		cy.server() // enable response stubbing
		cy.route('GET', `${vscodeServerBase}/test/tree`, 'fixture:normal-8/initialResponse.json')
		// cy.route('POST', `${vscodeServerBase}/loadNode`, 'fixture:normal-8/childrenOf4.json')
		//     .route('POST', `${vscodeServerBase}/loadNode`, 'fixture:normal-8/childrenOf4.json')

		cy.request(
			'http://localhost:5000/init//Users/tom/Documents/SearchTreeVisualisationTests/testData/diff/default-findAllSols-8/normal',
		)

		cy.visit('/singleTreeVisualisation.html')
	})

	it('Checks the root node is focussed', () => {
		cy.get('.selected')
	})

	it('Can go to the next node', () => {
		cy.get(':nth-child(3) > :nth-child(3)')
	})

	it('can collapse and expand without losing the decision labels', () => {
		cy.wait(1000)
		cy.get('#treeSVG').type('c', { delay: 500 })
		cy.get('#treeSVG').type('e', { delay: 500 })
		cy.get('[transform="translate(0, 90)"] > .decision').should('have.attr', 'style', 'fill-opacity: 1;')
	})
})
