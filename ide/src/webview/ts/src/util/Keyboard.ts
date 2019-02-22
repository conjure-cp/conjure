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
            if (Globals.data.id2Node[Globals.data.selectedId].children) {
                if (Globals.data.id2Node[Globals.data.selectedId].children.length > 1) {
                    Globals.nextNode();
                }
            }
        }, 'keydown');

        Mousetrap.bind('t', () => {
            Globals.data.toggleNode(Globals.data.selectedId);
            Tree.update(Globals.data.id2Node[Globals.data.selectedId]);
        }, 'keydown');

        Mousetrap.bind('c', () => {
            Globals.data.collapseNode(Globals.data.selectedId);
            Tree.update(Globals.data.id2Node[Globals.data.selectedId]);
            Tree.selectNode(Globals.data.selectedId)
        }, 'keydown');

        Mousetrap.bind('e', () => {
            Globals.data.expandNode(Globals.data.selectedId);
            Tree.update(Globals.data.id2Node[Globals.data.selectedId]);
            Tree.selectNode(Globals.data.selectedId)
        }, 'keydown');

        Mousetrap.bind('m', () => {
            Globals.loadDomains();
        }, 'keydown');

        Mousetrap.bind('f', () => {
            Globals.data.collapseFailed();
            Tree.update(Globals.data.id2Node[Globals.data.selectedId]);
            Tree.selectNode(Globals.data.selectedId)
            Tree.update(Globals.data.id2Node[1]);
        }, 'keydown');
    }
}
