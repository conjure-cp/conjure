declare var d3: any;
import Globals from '../testable/Globals';
import Tree from './Tree';
import Node from '../testable/Node';
import State from '../testable/State';

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

    public static bindListener() {

        window.addEventListener('message', event => {
            const message = event.data;

            switch (message.command) {

                case 'loadSet':
                    Globals.lv.id2Node[message.data.structure.name].children = message.data.structure.children;
                    Globals.lv.updateFromRoot();
                    Globals.lv.updateNodes([message.data.update]);
                    Globals.sendPrettyRequest(Globals.vscode);
                    break;

                case 'init':
                    // console.log(message.data);
                    Listener.initHandler(message.data);
                    break;

                case 'longestBranchingVariable':

                    // console.log(message.data);
                    // Tree.tree.nodeSize([1000, Tree.nodeHeight]);
                    Tree.tree.nodeSize([Number(message.data) * 13, Tree.nodeHeight]);
                    break;

                case 'loadNodes':

                    // console.log(message.data);

                    message.data.forEach((element: any) => {

                        if (!State.id2Node[element.id]) {
                            State.addNode(element.parentId, element);
                            State.totalLoaded++;
                        }
                    });

                    Tree.update(State.id2Node[State.rootId]);

                    Tree.selectNode(State.selectedId);

                    Listener.setLoadedCount();

                    break;

                case 'simpleDomains':

                    // console.log(message.data.changedNames);    

                    message.data.vars.forEach((variable: any) => {

                        // console.log(message.data.changedNames);

                        $("#" + $.escapeSelector(variable.name)).removeClass("changed");

                        let li = $("#" + $.escapeSelector(variable.name) + " > :last-child");
                        li.text(variable.rng);

                        if (message.data.changedNames.includes(variable.name)) {
                            d3.select('[id="' + $.escapeSelector(variable.name) + '"]').classed("changed", true);
                            // $("#" + $.escapeSelector(variable.name)).toggleClass("changed");
                        }
                    });
                    break;

                case 'prettyDomains':
                    // console.log(message.data);

                    Globals.lv.setChangedExpressions(message.data.changedExpressions);
                    Globals.lv.updateNodes(message.data.vars);
                    Globals.lv.setChanged(message.data.changed);
                    break;
            }
            State.waiting = false;
        });

    }
}
