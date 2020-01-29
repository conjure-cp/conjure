const prdVscodeServerURL = 'http://localhost:8000'
const url = `${prdVscodeServerURL}/config/solve`

let sampleSetup: any

describe('testing the solve endpoint', () => {
	beforeEach(() => {
		sampleSetup = {
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
		}
	})

	it('Makes sure the page can be getted', () => {
		cy.visit(prdVscodeServerURL)
	})

	it('Makes sure we can send a solve request', () => {
		cy.request('POST', url, [ sampleSetup ]).then((response) => {
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

	it('Checks an error is returned if there is something wring with the essence', () => {
		sampleSetup.essenceFile = 'zzz_broken.essence'
		cy.request('POST', url, [ sampleSetup ]).then((response) => {
			expect(response.body.stackTrace).to.contain('Error: Undefined reference to a domain: dNum')
		})
	})
})

// TODO sometimes get errors from the nim server when re solving a problem that has already be done before and has since has its caches invalidated.
