import State from './State';
import Node from './Node';
import Globals from './Globals';

export default class Navigate {

    public static nextNode(vscodeApi: any) {

        let stepSize = 1;
        let node = State.id2Node[State.selectedId];

        if (node._children) {
            Node.toggleNode(node);
            return;
        }

        if (!State.id2Node[State.selectedId + stepSize]) {
            Globals.loadNodes(vscodeApi);
        }
        else {
            State.selectedId += stepSize;
        }
    }

    public static previousNode() {

        let prevId = State.selectedId - 1;

        if (State.id2Node[prevId]) {
            State.selectedId--;
        }
    }

    public static rightNode() {
        let node = State.id2Node[State.selectedId];
        if (node.children) {
            if (node.children.length > 1) {
                State.selectedId = node.children![1].id;
            }
        }
    }

    public static leftNode() {
        let node = State.id2Node[State.selectedId];
        if (node.children) {
            if (node.children.length > 1) {
                State.selectedId = (node.children[0].id);
            }
        }
    }

    public static upNode() {
        if (State.selectedId > State.rootId) {
            State.selectedId = State.id2Node[State.selectedId].parent!.id;
        }
    }

    public static nextSolutionNode() {

        if (State.solNodIds.length === 0) {
            return;
        }

        if (!State.solNodIds.includes(State.selectedId)) {
            State.selectedId = State.solNodIds[0];
            return;
        }

        let currentSolId = State.solNodIds.indexOf(State.selectedId);

        if (currentSolId + 1 < State.solNodIds.length) {
            State.selectedId = State.solNodIds[currentSolId + 1];
        }
    }

    public static previousSolutionNode() {

        if (State.solNodIds.length === 0) {
            return;
        }

        if (!State.solNodIds.includes(State.selectedId)) {
            State.selectedId = State.solNodIds[State.solNodIds.length - 1];
            return;
        }

        let currentSolId = State.solNodIds.indexOf(State.selectedId);

        if (currentSolId - 1 >= 0) {
            State.selectedId = State.solNodIds[currentSolId - 1];
        }
    }
}
