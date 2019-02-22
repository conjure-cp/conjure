export default class Node{
    public id: number;
    public name: string;
    public parent: Node | null;
    public children: Node[] | null;
    public _children: Node[] | null;
    public x: number;
    public y: number;

    constructor(id:number, name: string, parent: Node){
        this.id = id;
        this.name = name;
        this.parent = parent;
        this.children = null;
        this._children = [];
        this.x = 0;
        this.y = 0;
    }
}