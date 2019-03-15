import Node from './Node';

export default class State {
    public static totalLoaded = 0;
    public static rootId = 0;
    public static selectedId = State.rootId;
    public static currentDomainId = 0;
    public static id2Node: { [id: number]: Node; } = {};
    public static solAncestorIds: number[] = [];
    public static solNodIds: number[] = [];
    public static pathList: string[] = [];
    public static simpleDomainsAtRoot: any;
    public static init = true;
    public static pretty = true;
    public static frozen = false;
    public static waiting = false;

    public static collapseFailed() {
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

    // public static addNode(nodeId: number, parentId: number, label: string, prettyLabel: string, decCount: number, isLeftChild: boolean, childCount: number, isSolution: boolean) {
    public static addNode(parentId: number, newNode: Node) {

        // console.log(parentId);
        // console.log(this.id2Node[parentId]);

        // let newNode = new Node(nodeId, label, prettyLabel, this.id2Node[parentId], decCount, isLeftChild, childCount, isSolution);

        if (parentId === -1) {
            // console.log("nodeId is")
            this.id2Node[newNode.id] = newNode;
            // console.log(this.id2Node);
            return;
        }

        if (!this.id2Node[parentId].children) {
            this.id2Node[parentId].children = [];
        }

        if (newNode.isLeftChild){
            this.id2Node[parentId].children!.unshift(newNode);
        }
        else{
            this.id2Node[parentId].children!.push(newNode);
        }
        // this.id2Node[parentId].children.push(newNode);

        this.id2Node[newNode.id] = newNode;
        // console.log(this.id2Node);


        // console.log("adding");
        // console.log(nodeId);
        // console.log(this.id2Node[1]);
    }

}