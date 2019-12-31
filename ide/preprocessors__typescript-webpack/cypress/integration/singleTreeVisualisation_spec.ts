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

	it('can load all the nodes in the tree', () => {
		cy.wait(1000)
		cy.get('#treeSVG').type('c')

		cy.get(':nth-child(1) > .card-header > .collapsed').click()

		cy
			.get(
				':nth-child(3) > [style="position: relative; width: 100%;"] > .slider-handles > [style="left: 25%; position: absolute; margin-left: -5px; margin-top: 35px; z-index: 2; width: 10px; height: 10px; border: 0px; text-align: center; cursor: pointer; border-radius: 50%; background-color: red; color: black;"]',
			)
			.trigger('mousedown', { which: 1, pageX: 600, pageY: 0 })
			.trigger('mousemove', { which: 1, pageX: -600, pageY: 0 })
			.trigger('mouseup')

		for (let i = 0; i < 66; i++) {
			cy.get('#treeSVG').type('s', { delay: 0 })
		}

		cy.get('#tree0thegroup g circle').should('have.length', 33)
	})
})
