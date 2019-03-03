import Node from './Node';

export default class State {
    public totalLoaded = 0;
    public rootId = 0;
    public selectedId = this.rootId;
    public currentDomainId = 0;
    public id2Node: { [id: number]: Node; } = {};
    public id2ChildIds: { [id: number]: [number]; } = {};
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
            let childIds = this.id2ChildIds[nodeId];
            childIds.forEach((childId: number) => {
                if (!this.solAncestorIds.includes(childId)) {
                    Node.collapseNode(this.id2Node[childId]);
                }
            });
        });

        let correctAncestor: Node = null;

        let recurse = (node: Node) => {
            if (this.solAncestorIds.includes(node.id)) {
                correctAncestor = node;
                return;
            }
            recurse(node.parent);
        };

        recurse(this.id2Node[this.selectedId]);

        this.selectedId = correctAncestor.id;
    }

    public addNode(nodeId: number, parentId: number, label: string, prettyLabel: string, decCount: number) {


        this.totalLoaded++;
        let newNode = new Node(nodeId, label, prettyLabel, this.id2Node[parentId], decCount);

        if (parentId === -1) {
            this.id2Node[nodeId] = newNode;
            return;
        }

        if (!this.id2Node[parentId].children) {
            this.id2Node[parentId].children = [];
        }

        this.id2Node[parentId].children.push(newNode);
        this.id2Node[nodeId] = newNode;
    }

}