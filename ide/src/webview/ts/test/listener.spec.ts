import Listener from '../src/util/Listener';
import State from '../src/testable/State';

describe('Test the Listener class', function () {

    beforeEach(function () {
        State.id2Node = {};
        State.solAncestorIds = [];
        State.solNodIds = [];
        State.rootId = 0;
        State.selectedId = State.rootId;
        State.waiting = false;
        State.pretty = true;
    });


    describe('Init handler ', function () {

        // let node1 = ;
        let core = {
            nodes: [{ "childCount": 1, "decCount": 36, "id": 0, "isLeftChild": true, "isSolution": false, "label": "", "parentId": -1, "prettyLabel": "" },
                { "childCount": 2, "decCount": 35, "id": 1, "isLeftChild": true, "isSolution": false, "label": "Root Propagation", "parentId": 0, "prettyLabel": "Root Propagation" }],
            solAncestorIds: [1, 2, 3]
        };

        let fakeResponse = {
            simpleAtRoot: { vars: { "name": "x", "rng": "int(1..10)" } },
            prettyAtRoot: {"blah":"blah"},
            core: core,
        };

        it('Should updateTheState', function () {
            Listener.initHandler(fakeResponse);
            expect(State.solAncestorIds).toBe(core.solAncestorIds);
            expect(State.simpleDomainsAtRoot).toBe(fakeResponse.simpleAtRoot.vars);
        });
    });


});
