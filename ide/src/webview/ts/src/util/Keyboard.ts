declare var Mousetrap: any;
import Globals from './Globals';
import Tree from './Tree';
import Node from './Node';

export default class Keyboard {

    public static bindKeys() {

        Mousetrap.bind('r', () => {
            Tree.selectNode(Globals.s.rootId);
        }, 'keydown');

        Mousetrap.bind('s', () => {
            Globals.nextNode();
            Tree.update(Globals.s.id2Node[Globals.s.selectedId]);
        }, 'keydown');

        Mousetrap.bind('w', Globals.upNode, 'keydown');

        Mousetrap.bind('shift', Globals.previousNode, 'keydown');

        Mousetrap.bind('d', Globals.rightNode, 'keydown');

        Mousetrap.bind('a', () => {
            let node = Globals.s.id2Node[Globals.s.selectedId];
            if (node.children) {
                if (node.children.length > 1) {
                    Tree.selectNode(node.children[0].id);
                }
            }
        }, 'keydown');

        Mousetrap.bind('t', () => {
            let node = Globals.s.id2Node[Globals.s.selectedId];
            Node.toggleNode(node);
            Tree.update(node);
        }, 'keydown');

        Mousetrap.bind('c', () => {
            let node = Globals.s.id2Node[Globals.s.selectedId];
            Node.collapseNode(node);
            Tree.selectNode(node.id);
            Tree.update(node);
        }, 'keydown');

        Mousetrap.bind('e', () => {
            let node = Globals.s.id2Node[Globals.s.selectedId];
            Node.expandNode(node);
            Tree.selectNode(node.id);
            Tree.update(node);
        }, 'keydown');

        Mousetrap.bind('m', () => {
            Globals.loadDomains();
        }, 'keydown');

        Mousetrap.bind('f', () => {
            Globals.s.collapseFailed();
            let node = Globals.s.id2Node[Globals.s.selectedId];
            Tree.update(node);
            Tree.selectNode(node.id);
            Tree.update(Globals.s.id2Node[Globals.s.rootId]);
        }, 'keydown');
    }
}
