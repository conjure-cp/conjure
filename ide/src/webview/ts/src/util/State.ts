import Node from '../testable/Node';
// import { id2Node } from './globals';

export default class State {
    public totalLoaded = 0;
    public rootId = 0;
    public selectedId = this.rootId;
    public currentDomainId = 0;
    public id2Node: { [id: number]: Node; } = {};
    // public id2ChildIds: { [id: number]: [number]; } = {};
    public solAncestorIds: number[] = [];
    public solNodIds: number[] = [];
    public pathList: string[] = [];
    public simpleDomainsAtRoot: any;
    public init = true;
    public pretty = true;
    public frozen = false;
    public waiting = false;

    constructor() { }

    public collapseFailed() {
        this.solAncestorIds.forEach((nodeId: number) => {

            let current = this.id2Node[nodeId];

            if (!current.children){
                return;
            }
            // let childIds = this.id2ChildIds[nodeId];
            current.children.forEach((child: Node) => {
                if (!this.solAncestorIds.includes(child.id)) {
                    Node.collapseNode(child);
                }
            });
        });

        // let correctAncestor: Node;

        let recurse = (node: Node) => {
            if (this.solAncestorIds.includes(node.id)) {
                this.selectedId = node.id;
                // correctAncestor = node;
                return;
            }

            if (node.parent === null){
                return;
            }

            recurse(node.parent);
        };

        recurse(this.id2Node[this.selectedId]);

    }

    public addNode(nodeId: number, parentId: number, label: string, prettyLabel: string, decCount: number, isLeftChild: boolean, childCount: number, isSolution: boolean) {

        // console.log(parentId);
        // console.log(this.id2Node[parentId]);

        let newNode = new Node(nodeId, label, prettyLabel, this.id2Node[parentId], decCount, isLeftChild, childCount, isSolution);

        if (parentId === -1) {
            // console.log("nodeId is")
            this.id2Node[nodeId] = newNode;
            // console.log(this.id2Node);
            return;
        }

        if (!this.id2Node[parentId].children) {
            this.id2Node[parentId].children = [];
        }

        if (isLeftChild){
            this.id2Node[parentId].children!.unshift(newNode);
        }
        else{
            this.id2Node[parentId].children!.push(newNode);
        }
        // this.id2Node[parentId].children.push(newNode);

        this.id2Node[nodeId] = newNode;
        // console.log(this.id2Node);


        // console.log("adding");
        // console.log(nodeId);
        // console.log(this.id2Node[1]);
    }

}