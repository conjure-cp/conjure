import Node from './Node';

export default class Data {
    public totalLoaded = 0;
    public rootId = 0;
    public selectedId = this.rootId;
    public currentDomainId = 0;
    public id2Node: {[id: number] : Node;} = {};
    public id2ChildIds: any = {};
    public correctPath: any[] = [];
    public pathList: any[] = [];
    public simpleDomainsAtRoot: any;
    public init = true;
    public pretty = true;
    public frozen = false;
    public waiting = false;
    public columns = ["name", "rng"];

    constructor(){
    }

    public expandNode (nodeId: number) {

        let recurse = (node: any) => {

            for (var i in node._children) {
                recurse(node._children[i]);
            }

            this.showChildren(node.id);
        }

        let node = this.id2Node[nodeId];
        recurse(node);
    }

    public collapseNode (nodeId: number) {


        let recurse = (node: Node) => {

            for (var i in node.children) {
                recurse(node.children[i]);
            }

            this.hideChildren(node.id);
        };

        let node = this.id2Node[nodeId];
        recurse(node);

    }

    public showChildren(nodeId: number) {

        if (this.id2Node[nodeId]) {
            if (this.id2Node[nodeId]._children) {
                this.id2Node[nodeId].children = this.id2Node[nodeId]._children;
                this.id2Node[nodeId]._children = null;
            }
        }
    }

    public hideChildren(nodeId: number) {
        if (this.id2Node[nodeId]) {
            if (this.id2Node[nodeId].children) {
                this.id2Node[nodeId]._children = this.id2Node[nodeId].children;
                this.id2Node[nodeId].children = null;
            }
        }
    }

    public toggleNode (nodeId: number) {

        if (this.id2Node[nodeId]._children) {
            this.showChildren(nodeId);
        }
        else if (this.id2Node[nodeId].children) {
            this.hideChildren(nodeId);
        }
    }

    public collapseFailed() {
        this.correctPath.forEach((nodeId: number) => {
            if (this.id2ChildIds[nodeId]) {
                this.id2ChildIds[nodeId].forEach((childId: number) => {
                    if (!this.correctPath.includes(childId)) {
                        this.collapseNode(childId);
                    }
                });
            }

        });

        if (!this.correctPath.includes(this.selectedId)) {

            for (var i = 0; i < this.correctPath.length; i++) {
                let nodeId = this.correctPath[i];

                if (nodeId > this.selectedId) {
                    this.selectedId = nodeId;
                    break;
                }
            }
        }
    }


    public addNode (nodeId: number, parentId: number, label: string) {

        this.totalLoaded++;
        // let newNode = { id: nodeId, name: label, parent: this.id2Node[parentId] };
        let newNode = new Node(nodeId, label, this.id2Node[parentId]);
        // console.log(exports.currentId);
        // console.log(parentId);
        // console.log(exports.id2Node);

        if (parentId === -1) {
            this.id2Node[nodeId] = newNode;
            return;
        }

        if (!this.id2Node[parentId].children) {
            this.id2Node[parentId].children = [];
        }

        this.id2Node[parentId].children.push(newNode);
        this.id2Node[nodeId] = newNode;
        // exports.id2Parent[nodeId] = exports.id2Node[parentId];
    }

}