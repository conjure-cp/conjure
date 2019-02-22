declare var Mousetrap: any;
import Globals from './Globals';
import Tree from './Tree';

export default class Keyboard {

    public static bindKeys() {

        Mousetrap.bind('s', () => {
            Globals.nextNode();
            Tree.update(Globals.data.id2Node[Globals.data.selectedId]);
        }, 'keydown');

        Mousetrap.bind('w', Globals.upNode, 'keydown');

        Mousetrap.bind('shift', Globals.previousNode, 'keydown');

        Mousetrap.bind('d', Globals.rightNode, 'keydown');

        Mousetrap.bind('a', () => {

            let node = Globals.data.id2Node[Globals.data.selectedId];

            if (node.children) {
                if (node.children.length > 1) {
                    Tree.selectNode(node.children[0].id);
                }
            }

        }, 'keydown');

        Mousetrap.bind('t', () => {
            let node = Globals.data.id2Node[Globals.data.selectedId];
            Globals.data.toggleNode(node.id);
            Tree.update(node);
        }, 'keydown');

        Mousetrap.bind('c', () => {
            let node = Globals.data.id2Node[Globals.data.selectedId];
            Globals.data.collapseNode(node.id);
            Tree.selectNode(node.id);
            Tree.update(node);
        }, 'keydown');

        Mousetrap.bind('e', () => {
            let node = Globals.data.id2Node[Globals.data.selectedId];
            Globals.data.expandNode(node.id);
            Tree.update(node);
            Tree.selectNode(node.id);
        }, 'keydown');

        Mousetrap.bind('m', () => {
            Globals.loadDomains();
        }, 'keydown');

        Mousetrap.bind('f', () => {
            Globals.data.collapseFailed();
            let node = Globals.data.id2Node[Globals.data.selectedId];
            Tree.update(node.id);
            Tree.selectNode(node.id);
            Tree.update(Globals.data.id2Node[Globals.data.rootId]);
        }, 'keydown');
    }
}
