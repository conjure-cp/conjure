export default class Node {
    public id: number;
    public name: string;
    public parent: Node | null;
    public children: Node[] | null;
    public _children: Node[] | null;
    public x: number;
    public y: number;
    public x0: number | null;
    public y0: number | null;
    public depth: number;
    public decCount: number = 0;

    constructor(id: number, name: string, parent: Node) {
        this.id = id;
        this.name = name;
        this.parent = parent;
        this.children = null;
        this._children = null;
        this.x = 0;
        this.y = 0;
        this.x0 = null;
        this.y0 = null;
        this.depth = 0;
    }
    public static expandNode(node: Node) {

        let recurse = (insideNode: Node) => {

            for (var i in insideNode._children) {
                recurse(insideNode._children[i]);
            }

            Node.showChildren(insideNode);
        };

        recurse(node);
    }

    public static collapseNode(node: Node) {


        let recurse = (insideNode: Node) => {

            for (var i in insideNode.children) {
                recurse(insideNode.children[i]);
            }

            Node.hideChildren(insideNode);
        };

        recurse(node);

    }

    public static showChildren(node: Node) {
        if (node) {
            if (node._children) {
                node.children = node._children;
                node._children = null;
            }
        }
    }

    public static hideChildren(node: Node) {
        if (node) {
            if (node.children) {
                node._children = node.children;
                node.children = null;
            }
        }
    }

    public static toggleNode(node: Node) {

        if (node._children) {
            Node.showChildren(node);
        }
        else if (node.children) {
            Node.hideChildren(node);
        }
    }

}