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

        it('Pick a config', () => {
            // cy.pause()
            const c2 = cachesJson[1]

            cy.get('.css-1hwfws3').click()
            cy.get('.css-26l3qy-menu').click()
            cy.get('select[name="caches[0].essenceFile"]').should('have.value', c2.essenceFile)
            cy.get('select[name="caches[0].paramFile"]').should('have.value', c2.paramFile)

            cy
                .get('#conjure1 > .card-body > .row > .col > .input-group')
                .should('have.value', c2.config.conjureConfig.conjureTime)
            cy.get('#ChooseRepresentation-check').should('be.checked')
            cy.contains('x')
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
        })

        // it('Pick a config', () => {
        //     // cy.pause()
        //     cy.get('.css-1hwfws3').click()
        //     cy.get('.css-26l3qy-menu').click()
        //     cy.get('select[name="caches[0].essenceFile"]').should('have.value', 'cached2.essence')
        //     cy.get('select[name="caches[0].paramFile"]').should('have.value', 'cached2.param')
        //     cy.get('#conjure1 > .card-body > .row > .col > #Timelimit-textBox').should('have.value', '200')
        //     cy.get('#Strategy-select').should('have.value', 's')
        // })
    })
})
