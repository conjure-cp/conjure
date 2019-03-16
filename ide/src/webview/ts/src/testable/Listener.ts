declare var d3: any;
import Globals from './Globals';
import Tree from './Tree';
import Node from './Node';
import State from './State';

export default class Listener {

    public static setLoadedCount() {
        $("#total").text(State.totalLoaded + "/" + Number(State.id2Node[State.rootId].decCount + 1));
    }

    public static initHandler(data: any) {
        Globals.lv.update(data.prettyAtRoot);

        State.simpleDomainsAtRoot = data.simpleAtRoot.vars;
        State.solAncestorIds = data.core.solAncestorIds;
        State.totalLoaded += data.core.nodes.length;

        for (let i = 0; i < data.core.nodes.length; i++) {
            let element = data.core.nodes[i];
            State.addNode(element.parentId, element);
        }

        Node.collapseNode(State.id2Node[State.rootId]);
        Tree.update(State.id2Node[State.rootId]);
        Tree.selectNode(State.rootId);
        Listener.setLoadedCount();
    }

    public static longestBranchingVariableHandler(data: any) {
        Tree.tree.nodeSize([Number(data) * 13, Tree.nodeHeight]);
    }

    public static loadNodesHandler(data: any) {
        data.forEach((element: any) => {

            if (!State.id2Node[element.id]) {
                State.addNode(element.parentId, element);
                State.totalLoaded++;
            }
        });

        Tree.update(State.id2Node[State.rootId]);

        Tree.selectNode(State.selectedId);

        Listener.setLoadedCount();
    }

    public static loadSetHandler(data: any) {
        Globals.lv.id2Node[data.structure.name].children = data.structure.children;
        Globals.lv.updateFromRoot();
        Globals.lv.updateNodes([data.update]);
        Globals.sendPrettyRequest(Globals.vscode);
    }

    public static simpleDomainsHandler(data: any) {
        data.vars.forEach((variable: any) => {

            $("#" + $.escapeSelector(variable.name)).removeClass("changed");

            let li = $("#" + $.escapeSelector(variable.name) + " > :last-child");
            li.text(variable.rng);

            if (data.changedNames.includes(variable.name)) {
                d3.select('[id="' + $.escapeSelector(variable.name) + '"]').classed("changed", true);
            }
        });
    }

    public static prettyDomainsHandler(data: any){
        Globals.lv.setChangedExpressions(data.changedExpressions);
        Globals.lv.updateNodes(data.vars);
        Globals.lv.setChanged(data.changed);
    }

    public static bindListener() {

        window.addEventListener('message', event => {
            const message = event.data;

            switch (message.command) {

                case 'loadSet':
                    Listener.loadSetHandler(message.data);
                    break;

                case 'init':
                    Listener.initHandler(message.data);
                    break;

                case 'longestBranchingVariable':
                    Listener.longestBranchingVariableHandler(message.data);
                    break;

                case 'loadNodes':
                    Listener.loadNodesHandler(message.data);
                    State.waiting = false;
                    break;

                case 'simpleDomains':
                    Listener.simpleDomainsHandler(message.data);
                    break;

                case 'prettyDomains':
                    Listener.prettyDomainsHandler(message.data);
                    break;
            }
        });

    }
}
