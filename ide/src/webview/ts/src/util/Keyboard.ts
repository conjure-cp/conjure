declare var Mousetrap: any;
import Globals from '../testable/Globals';
import Tree from '../testable/Tree';
import Node from '../testable/Node';
import State from '../testable/State';
import Navigate from '../testable/Navigate';

export default class Keyboard {

    public static bindKeys() {
        Mousetrap.bind(']', () => {
            Node.collapseNode(State.id2Node[State.rootId]);
            Node.expandNode(State.id2Node[State.rootId]);
            Tree.update(State.id2Node[State.rootId]);
            Navigate.nextSolutionNode();
            Tree.selectNode(State.selectedId);
        }, 'keydown');

        Mousetrap.bind('[', () => {
            Node.collapseNode(State.id2Node[State.rootId]);
            Node.expandNode(State.id2Node[State.rootId]);
            Tree.update(State.id2Node[State.rootId]);
            Navigate.previousSolutionNode();
            Tree.selectNode(State.selectedId);
        }, 'keydown');

        Mousetrap.bind('r', () => {
            Tree.selectNode(State.rootId);
        }, 'keydown');

        Mousetrap.bind('s', () => {
            Navigate.nextNode(Globals.vscode);
            Tree.selectNode(State.selectedId);
            Tree.update(State.id2Node[State.selectedId]);
        }, 'keydown');

        Mousetrap.bind('w', () => {
            Navigate.upNode();
            Tree.selectNode(State.selectedId);
        }, 'keydown');

        Mousetrap.bind('shift', () => {
            Navigate.previousNode();
            Tree.selectNode(State.selectedId);
        }, 'keydown');

        Mousetrap.bind('d', () => {
            Navigate.rightNode();
            Tree.selectNode(State.selectedId);
        },  'keydown');

        Mousetrap.bind('a', () => {
            Navigate.leftNode();
            Tree.selectNode(State.selectedId);
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

        Mousetrap.bind('f', () => {
            State.collapseFailed();
            let node = State.id2Node[State.selectedId];
            Tree.update(node);
            Tree.selectNode(node.id);
            Tree.update(State.id2Node[State.rootId]);
        }, 'keydown');
    }
}
