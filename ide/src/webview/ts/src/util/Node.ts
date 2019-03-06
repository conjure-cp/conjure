export default class Node {
    public id: number;
    public label: string;
    public name: string;
    public prettyLabel: string;
    public parent: Node | null;
    public children: Node[] | null;
    public _children: Node[] | null;
    public x: number;
    public y: number;
    public x0: number | null;
    public y0: number | null;
    public depth: number;
    public decCount: number = 0;
    public isLeftChild: boolean;
    public childCount : number;
    public isSolution: boolean;

    constructor(id: number, name: string, prettyLabel: string, parent: Node, decCount: number, isLeftchild: boolean, childCount: number, isSolution: boolean) {
        this.id = id;
        this.parent = parent;
        this.children = null;
        this._children = null;
        this.x = 0;
        this.y = 0;
        this.x0 = null;
        this.y0 = null;
        this.depth = 0;
        this.decCount = decCount;
        this.prettyLabel = prettyLabel;
        this.isLeftChild = isLeftchild;
        this.childCount = childCount;
        this.isSolution = isSolution
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

    public static hasMoreChildren(node: Node): boolean{
        let childLength = 0;
        if (node.children) {
            childLength = node.children.length;
        }

        return (childLength < node.childCount);
    }


    public static calculateRadius(node: Node): number{
        // console.log(node);


        if (node.decCount === 0 || node.children && node.children.length === node.childCount){
            return 10;
        }

        return Math.log(node.decCount + 10) * 3;

    }

}