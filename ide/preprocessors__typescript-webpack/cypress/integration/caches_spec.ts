// import function "add" from another TypeScript file

const cachesJson = require('../fixtures/caches.json')

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
			cy.contains('Conjure').click()
			cy.contains('Savile Row').click()
			cy.contains('a', 'Minion').click()
		})

		// What about configs without answers?

		// This is only for config2, we still need to config 1

		it('Pick config 1', () => {
			const c1 = cachesJson[0]

			cy.get('.css-1hwfws3').type('Config1')
			cy.get('#react-select-2-option-0').click()

			// cy.get('#conjure1 > .card-body > .row > .col > .input-group').type('123456')

			cy.get('select[name="caches[0].essenceFile"]').should('have.value', c1.essenceFile)
			cy.get('select[name="caches[0].paramFile"]').should('have.value', c1.paramFile)

			cy
				.get('#conjure1 > .card-body > .row > .col > .input-group')
				.should('have.value', c1.config.conjureConfig.conjureTime)
			cy.get('#ChooseRepresentation-check').should('not.be.checked')
			cy.get('#Optimisation-select').should('have.value', c1.config.srConfig.optimisation)
			cy.get('#Symmetry\\ Breaking-select').should('have.value', c1.config.srConfig.symmetry)
			cy.get('#Translation-select').should('have.value', c1.config.srConfig.translation)
			cy
				.get('#sr1 > .card-body > :nth-child(4) > .col > .input-group')
				.should('have.value', c1.config.srConfig.srTime)
			cy
				.get('#sr1 > .card-body > :nth-child(5) > .col > .input-group')
				.should('have.value', c1.config.srConfig.cnfLimit)

			cy.get('#Findallsolutions-Checkbox').should('not.be.checked')

			cy.get('#RandomiseVarOrder-Checkbox').should('not.be.checked')
			cy.get(':nth-child(3) > .col > .input-group').should('have.value', c1.config.minionConfig.minionTime)
			cy
				.get('#minion1 > .card-body > :nth-child(4) > .col > .input-group')
				.should('have.value', c1.config.minionConfig.nodeLimit)
			cy
				.get('#minion1 > .card-body > :nth-child(5) > .col > .input-group')
				.should('have.value', c1.config.minionConfig.solLimit)
			cy.get('#Preprocessing-select').should('have.value', c1.config.minionConfig.preprocessing)
			cy.get('#Consistency-select').should('have.value', c1.config.minionConfig.consistency)
		})

		it('Pick config 2', () => {
			const c2 = cachesJson[1]

			cy.get('.css-1hwfws3').click()
			cy.get('.css-26l3qy-menu').click()
			cy.get('select[name="caches[0].essenceFile"]').should('have.value', c2.essenceFile)
			cy.get('select[name="caches[0].paramFile"]').should('have.value', c2.paramFile)

			cy
				.get('#conjure1 > .card-body > .row > .col > .input-group')
				.should('have.value', c2.config.conjureConfig.conjureTime)
			cy.get('#ChooseRepresentation-check').should('be.checked')
			cy.get('#conjure1 > .card-body > :nth-child(3) > label').should('exist')
			cy.get('#x-select').should('have.value', '1')
			cy.get('#Optimisation-select').should('have.value', c2.config.srConfig.optimisation)
			cy.get('#Symmetry\\ Breaking-select').should('have.value', c2.config.srConfig.symmetry)
			cy.get('#Translation-select').should('have.value', c2.config.srConfig.translation)
			cy
				.get('#sr1 > .card-body > :nth-child(4) > .col > .input-group')
				.should('have.value', c2.config.srConfig.srTime)
			cy
				.get('#sr1 > .card-body > :nth-child(5) > .col > .input-group')
				.should('have.value', c2.config.srConfig.cnfLimit)
			cy.get('#Findallsolutions-Checkbox').should('be.checked')
			cy.get('#RandomiseVarOrder-Checkbox').should('be.checked')
			cy.get(':nth-child(3) > .col > .input-group').should('have.value', c2.config.minionConfig.minionTime)
			cy
				.get('#minion1 > .card-body > :nth-child(4) > .col > .input-group')
				.should('have.value', c2.config.minionConfig.nodeLimit)
			cy
				.get('#minion1 > .card-body > :nth-child(5) > .col > .input-group')
				.should('have.value', c2.config.minionConfig.solLimit)
			cy.get('#Preprocessing-select').should('have.value', c2.config.minionConfig.preprocessing)
			cy.get('#Consistency-select').should('have.value', c2.config.minionConfig.consistency)
		})

		it('Pick config 1 and change a value', () => {
			const c1 = cachesJson[0]

			cy.get('.css-1hwfws3').type('Config1')
			cy.get('#react-select-2-option-0').click()

			cy.get('#conjure1 > .card-body > .row > .col > .input-group').clear().type('123456')

			cy.get('select[name="caches[0].essenceFile"]').should('have.value', c1.essenceFile)
			cy.get('select[name="caches[0].paramFile"]').should('have.value', c1.paramFile)

			cy.get('#conjure1 > .card-body > .row > .col > .input-group').should('have.value', '123456')
			cy.get('#ChooseRepresentation-check').should('not.be.checked')
			cy.get('#Optimisation-select').should('have.value', c1.config.srConfig.optimisation)
			cy.get('#Symmetry\\ Breaking-select').should('have.value', c1.config.srConfig.symmetry)
			cy.get('#Translation-select').should('have.value', c1.config.srConfig.translation)
			cy
				.get('#sr1 > .card-body > :nth-child(4) > .col > .input-group')
				.should('have.value', c1.config.srConfig.srTime)
			cy
				.get('#sr1 > .card-body > :nth-child(5) > .col > .input-group')
				.should('have.value', c1.config.srConfig.cnfLimit)

			cy.get('#Findallsolutions-Checkbox').should('not.be.checked')

			cy.get('#RandomiseVarOrder-Checkbox').should('not.be.checked')
			cy.get(':nth-child(3) > .col > .input-group').should('have.value', c1.config.minionConfig.minionTime)
			cy
				.get('#minion1 > .card-body > :nth-child(4) > .col > .input-group')
				.should('have.value', c1.config.minionConfig.nodeLimit)
			cy
				.get('#minion1 > .card-body > :nth-child(5) > .col > .input-group')
				.should('have.value', c1.config.minionConfig.solLimit)
			cy.get('#Preprocessing-select').should('have.value', c1.config.minionConfig.preprocessing)
			cy.get('#Consistency-select').should('have.value', c1.config.minionConfig.consistency)
		})

		it.only('When there are two trees the cache is applied approppriately ', () => {
			cy.get('#Comparetrees-check').click()
			cy.get('#config2 > :nth-child(1) > #cacheSelect > .css-yk16xz-control > .css-1hwfws3').type('Config1')
			cy.get('#react-select-3-option-1').click()
			cy.get('#config1 > :nth-child(1) > :nth-child(3) > #Model-select').should('have.value', 'model1.essence')
			cy.get('#config2 > :nth-child(1) > :nth-child(3) > #Model-select').should('have.value', 'cached1.essence')
		})
	})
})
