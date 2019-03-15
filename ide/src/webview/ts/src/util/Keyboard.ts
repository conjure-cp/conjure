declare var Mousetrap: any;
import Globals from './Globals';
import Tree from './Tree';
import Node from '../testable/Node';
import State from '../testable/State';

export default class Keyboard {

    public static bindKeys() {
        Mousetrap.bind(']', () => {
            Node.collapseNode(State.id2Node[State.rootId]);
            Node.expandNode(State.id2Node[State.rootId]);
            Tree.update(State.id2Node[State.rootId]);
            Globals.nextSolutionNode();
            Tree.selectNode(State.selectedId);
        }, 'keydown');

        Mousetrap.bind('[', () => {
            Node.collapseNode(State.id2Node[State.rootId]);
            Node.expandNode(State.id2Node[State.rootId]);
            Tree.update(State.id2Node[State.rootId]);
            Globals.previousSolutionNode();
            Tree.selectNode(State.selectedId);
        }, 'keydown');

        Mousetrap.bind('r', () => {
            Tree.selectNode(State.rootId);
        }, 'keydown');

        Mousetrap.bind('s', () => {
            Globals.nextNode();
            Tree.update(State.id2Node[State.selectedId]);
        }, 'keydown');

        Mousetrap.bind('w', Globals.upNode, 'keydown');

        Mousetrap.bind('shift', Globals.previousNode, 'keydown');

        Mousetrap.bind('d', Globals.rightNode, 'keydown');

        Mousetrap.bind('a', () => {
            let node = State.id2Node[State.selectedId];
            if (node.children) {
                if (node.children.length > 1) {
                    Tree.selectNode(node.children[0].id);
                }
            }
        }, 'keydown');

        Mousetrap.bind('t', () => {
            let node = State.id2Node[State.selectedId];
            Node.toggleNode(node);
            Tree.update(node);
        }, 'keydown');

        Mousetrap.bind('c', () => {
            let node = State.id2Node[State.selectedId];
            Node.collapseNode(node);
            Tree.update(node);
            Tree.selectNode(node.id);
        }, 'keydown');

        Mousetrap.bind('e', () => {
            let node = State.id2Node[State.selectedId];
            Node.expandNode(node);
            Tree.update(node);
            Tree.selectNode(node.id);
        }, 'keydown');

        Mousetrap.bind('m', () => {
            Globals.loadDomains();
        }, 'keydown');

        Mousetrap.bind('f', () => {
            State.collapseFailed();
            let node = State.id2Node[State.selectedId];
            Tree.update(node);
            Tree.selectNode(node.id);
            Tree.update(State.id2Node[State.rootId]);
        }, 'keydown');
    }
}
