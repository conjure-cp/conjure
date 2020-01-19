import { vscodeServerBase, delay } from '../support'

// import React from 'react's

// import Forest from '../../../src/webclient/src/components/Forest'

// const trees = require('../fixtures/normal-8/initialRespose.json')
describe('tree vis', () => {
	beforeEach('setup', () => {
		cy.request(
			'http://localhost:5000/init//Users/tom/Documents/SearchTreeVisualisationTests/testData/diff/default-findAllSols-8/normal',
		)
		cy.server() // enable response stubbing
		cy.route('GET', `${vscodeServerBase}/test/tree`, 'fixture:normal-8/initialResponse.json')
		cy.visit('/singleTreeVisualisation.html')
		cy.wait(delay)

		// cy.mount(<div></div>)

		// cy.mount(((
		// 	<Forest
		// 		trees={trees}
		// 		nimServerPort={5000}
		// 		requestHandler={() => {
		// 			console.log('req')
		// 		}}
		// 	/>
		// ) as unknown) as Symbol)
	})

	it('Checks that the next failed branch works', () => {
		cy.get('.row > :nth-child(1) > :nth-child(3)').click()
		cy.contains('Domains at 3')

		for (let i = 0; i < 40; i++) {
			cy.get('.row > :nth-child(1) > :nth-child(3)').click()
			cy.wait(delay)
		}

		cy.contains('Domains at 29')
	})

	it('Checks that the previous failed branch works', () => {
		cy.get('[tabindex="-1"] > .row > :nth-child(4) > :nth-child(2)').click()
		cy.get('[tabindex="-1"] > .row > :nth-child(1) > :nth-child(2)').click()
		cy.contains('Domains at 29')
		for (let i = 0; i < 40; i++) {
			cy.get('[tabindex="-1"] > .row > :nth-child(1) > :nth-child(2)').click()
			cy.wait(delay)
		}
		cy.contains('Domains at 3')
	})

	it('Checks that next solution node works', () => {
		cy.get(':nth-child(4) > :nth-child(3)').click()
		cy.contains('Domains at 32')
	})

	it('Checks that previous solution node works', () => {
		cy.get('[tabindex="-1"] > .row > :nth-child(4) > :nth-child(2)').click()
		cy.contains('Domains at 32')
	})

	it('Checks that the next solution branch works', () => {
		cy.get('.row > :nth-child(2) > :nth-child(3)').click()
		cy.contains('Domains at 1')
		cy.wait(delay)
		cy.get('.row > :nth-child(2) > :nth-child(3)').click()
		cy.contains('Domains at 2')
		cy.wait(delay)
		cy.get('.row > :nth-child(2) > :nth-child(3)').click()
		cy.contains('Domains at 16')
		cy.wait(delay)
		cy.get('.row > :nth-child(2) > :nth-child(3)').click()
		cy.contains('Domains at 26')
		cy.wait(delay)
		cy.get('.row > :nth-child(2) > :nth-child(3)').click()
		cy.contains('Domains at 27')
		cy.wait(delay)
		cy.get('.row > :nth-child(2) > :nth-child(3)').click()
		cy.contains('Domains at 30')
		cy.wait(delay)
		cy.get('.row > :nth-child(2) > :nth-child(3)').click()
		cy.contains('Domains at 31')
		cy.wait(delay)
		cy.get('.row > :nth-child(2) > :nth-child(3)').click()
		cy.contains('Domains at 32')
		cy.wait(delay)
	})

	it('Checks that the previous solution branch works', () => {
		cy.get('[tabindex="-1"] > .row > :nth-child(4) > :nth-child(2)').click()
		cy.contains('Domains at 32')
		cy.wait(delay)
		cy.get('[tabindex="-1"] > .row > :nth-child(2) > :nth-child(2)').click()
		cy.contains('Domains at 31')
		cy.wait(delay)
		cy.get('[tabindex="-1"] > .row > :nth-child(2) > :nth-child(2)').click()
		cy.contains('Domains at 30')
		cy.wait(delay)
		cy.get('[tabindex="-1"] > .row > :nth-child(2) > :nth-child(2)').click()
		cy.contains('Domains at 27')
		cy.wait(delay)
		cy.get('[tabindex="-1"] > .row > :nth-child(2) > :nth-child(2)').click()
		cy.contains('Domains at 26')
		cy.wait(delay)
		cy.get('[tabindex="-1"] > .row > :nth-child(2) > :nth-child(2)').click()
		cy.contains('Domains at 16')
		cy.wait(delay)
		cy.get('[tabindex="-1"] > .row > :nth-child(2) > :nth-child(2)').click()
		cy.contains('Domains at 2')
		cy.wait(delay)
		cy.get('[tabindex="-1"] > .row > :nth-child(2) > :nth-child(2)').click()
		cy.contains('Domains at 1')
		cy.wait(delay)
		cy.get('[tabindex="-1"] > .row > :nth-child(2) > :nth-child(2)').click()
	})

	it('Checks the labels can be toggled', () => {
		cy.get(':nth-child(1) > .card-header > .collapsed').click()
		cy.get('#ShowLabels-check').click()
		cy.get('#treeSVG').type('s', { delay: delay })
		cy.get('#treeSVG').type('s', { delay: delay })

		cy.get('.selected').siblings('text').should('have.text', '')
		cy.get('#ShowLabels-check').click()
		cy.get('.selected').siblings('text').should('not.have.text', '')
	})

	it('Checks the root node is focussed', () => {
		cy.get('.selected').siblings('text.decision').should('have.text', '')
	})

	it('Checks the nodes below is correct for the root', () => {
		cy.get('#treeSVG').type('c', { delay: delay })
		cy.get('.selected').siblings('text.descCount').should('contain.text', '32')
	})

	it('can collapse and expand without losing the decision labels', () => {
		cy.get('#treeSVG').type('c', { delay: delay })
		cy.get('#treeSVG').type('e', { delay: delay })
		cy.get('[transform="translate(0, 90)"] > .decision').should('have.attr', 'style', 'fill-opacity: 1;')
	})

	it('can load all the nodes in the tree', () => {
		cy.get('#treeSVG').type('c')

		// cy.get(':nth-child(1) > .card-header > .collapsed').click()

		// cy
		// 	.get(
		// 		':nth-child(3) > [style="position: relative; width: 100%;"] > .slider-handles > [style="left: 25%; position: absolute; margin-left: -5px; margin-top: 35px; z-index: 2; width: 10px; height: 10px; border: 0px; text-align: center; cursor: pointer; border-radius: 50%; background-color: red; color: black;"]',
		// 	)
		// 	.trigger('mousedown', { which: 1, pageX: 600, pageY: 0 })
		// 	.trigger('mousemove', { which: 1, pageX: -600, pageY: 0 })
		// 	.trigger('mouseup')

		for (let i = 0; i < 66; i++) {
			cy.get('#treeSVG').type('s', { delay: delay })
		}

		cy.get('#tree0thegroup g circle').should('have.length', 33)
	})

	it('can go to previous node by loading ancestors', () => {
		cy.get(':nth-child(4) > :nth-child(3)').click()

		cy.wait(delay)
		for (let i = 0; i < 3; i++) {
			cy.get('#treeSVG').type('{shift}', { delay: 0 })
			cy.wait(delay)
		}

		cy.contains('Domains at 29')
	})
})
