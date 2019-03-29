declare var Mousetrap: any;

import Globals from '../modules/Globals';
import Tree from '../modules/Tree';
import Node from '../modules/Node';
import State from '../modules/State';
import Navigate from '../modules/Navigate';

/**
 * This class binds key presses to actions
 */
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

        Mousetrap.bind(['s', 'down'], () => {
            Navigate.nextNode(Globals.vscode);
            Tree.selectNode(State.selectedId);
            Tree.update(State.id2Node[State.selectedId]);
        }, 'keydown');

        Mousetrap.bind(['w', 'up'], () => {
            Navigate.upNode();
            Tree.selectNode(State.selectedId);
        }, 'keydown');

        Mousetrap.bind('shift', () => {
            Navigate.previousNode();
            Tree.selectNode(State.selectedId);
        }, 'keydown');

        Mousetrap.bind(['d', 'right'], () => {
            Navigate.rightNode();
            Tree.selectNode(State.selectedId);
        }, 'keydown');

        Mousetrap.bind(['a', 'left'], () => {
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
