import { vscodeServerBase } from '../../support'

const prdVscodeServerURL = 'http://localhost:8000'

describe('testing the solve endpoint', () => {
	it('Makes sure the page can be getted', () => {
		cy.visit(prdVscodeServerURL)
	})

	it('Makes sure we can send a solve request', () => {
		// cy.request('GET', `${prdVscodeServerURL}`).then((response) => {
		// 	expect(response.body).to.have.property('trees') // true
		// })
		cy
			.request('POST', `${prdVscodeServerURL}/config/solve`, [
				{
					name: '',
					essenceFile: 'set_partition_simple.essence',
					paramFile: 'set_partition_simple-params/8.param',
					config: {
						conjureConfig: {
							conjureTime: '',
							strategy: '',
							answers: [],
						},
						srConfig: {
							optimisation: '',
							symmetry: '',
							translation: '',
							srTime: '',
							cnfLimit: '',
						},
						minionConfig: {
							nodeLimit: '',
							solLimit: '',
							minionTime: '',
							preprocessing: '',
							consistency: '',
							minionSwitches: [],
						},
					},
				},
			])
			.then((response) => {
				expect(response.body).to.have.property('trees')
				expect(response.body).to.have.property('nimServerPort')
				expect(response.body).to.have.property('vscodeServerPort')

				const { trees, nimServerPort, visit } = response.body

				expect(trees.length).to.equal(1)

				const tree = trees[0]

				expect(tree).to.have.property('hash')
				expect(tree).to.have.property('core')
				expect(tree).to.have.property('path')
				expect(tree).to.have.property('info')

				const core = tree.core

				expect(core.nodes.length).to.equal(12)
				expect(core.solAncestorIds.length).to.equal(9)
			})
	})
	// describe('Conjure', () => {
	// 	beforeEach('expand', () => {
	// 		cy.visit('/')
	// 		cy.contains('Conjure').click()
	// 	})

	// 	it('Checks can select answers', () => {
	// 		cy.get('input[id="ChooseRepresentation-check"]').click()
	// 		cy.contains('x')
	// 		cy.contains('explicit')
	// 	})
	// })
})
