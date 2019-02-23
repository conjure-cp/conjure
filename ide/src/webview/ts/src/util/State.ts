import Node from './Node';

export default class State {
    public totalLoaded = 0;
    public rootId = 0;
    public selectedId = this.rootId;
    public currentDomainId = 0;
    public id2Node: { [id: number]: Node; } = {};
    public id2ChildIds: any = {};
    public correctPath: any[] = [];
    public pathList: any[] = [];
    public simpleDomainsAtRoot: any;
    public init = true;
    public pretty = true;
    public frozen = false;
    public waiting = false;

    constructor() { }

    public collapseFailed() {
        this.correctPath.forEach((nodeId: number) => {
            let childIds = this.id2ChildIds[nodeId];
            childIds.forEach((childId: number) => {
                if (!this.correctPath.includes(childId)) {
                    Node.collapseNode(this.id2Node[childId]);
                }
            });
        });

        let correctAncestor : Node = null;

        let recurse = (node: Node) => {
            if (this.correctPath.includes(node.id)){
                correctAncestor = node;
                return;
            }
            recurse(node.parent);
        };

        recurse(this.id2Node[this.selectedId]);

        this.selectedId = correctAncestor.id;
    }

    public addNode(nodeId: number, parentId: number, label: string) {

        this.totalLoaded++;
        let newNode = new Node(nodeId, label, this.id2Node[parentId]);

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