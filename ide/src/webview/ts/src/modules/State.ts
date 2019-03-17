import Node from './Node';

/**
 * This class is used to keep track of the state of the webview.
 */

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

    /**
     * Collapse all failed branches
     */

    public static collapseFailed() {


        // Collapse the failed branches

        this.solAncestorIds.forEach((nodeId: number) => {
            let current = this.id2Node[nodeId];

            if (!current.children) {
                return;
            }
            current.children.forEach((child: Node) => {
                if (!this.solAncestorIds.includes(child.id)) {
                    Node.collapseNode(child);
                }
            });
        });

        // Move the selected node to one that is not within a failed branch.

        let recurse = (node: Node) => {

            if (this.solAncestorIds.includes(node.id)) {
                this.selectedId = node.id;
                return;
            }

            if (node.parent === null) {
                return;
            }

            recurse(node.parent);
        };

        recurse(this.id2Node[this.selectedId]);

    }

    /**
     * Add a node into the tree.
     * @param parentId The parent node's ID.
     * @param newNode The node to add.
     */

    public static addNode(parentId: number, newNode: Node) {

        if (newNode.isSolution) {
            State.solNodIds.push(newNode.id);
        }

        if (parentId === -1) {
            this.id2Node[newNode.id] = newNode;
            return;
        }

        if (!this.id2Node[parentId].children) {
            this.id2Node[parentId].children = [];
        }

        if (newNode.isLeftChild) {
            this.id2Node[parentId].children!.unshift(newNode);
        }
        else {
            this.id2Node[parentId].children!.push(newNode);
        }

        this.id2Node[newNode.id] = newNode;
    }

}