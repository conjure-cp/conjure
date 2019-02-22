import Listview from './Listview';
import Globals from './Globals';
import Tree from './Tree';
import Node from './Node';

export default class Listener {

    public static setLoadedCount() {
        $("#total").text(Globals.s.totalLoaded + "/" + "?");
    }

    public static bindListener() {

        let init = true;

        window.addEventListener('message', event => {
            const message = event.data;


            switch (message.command) {

                case 'loadChildren':
                    let nodeId = message.data.nodeId;
                    Globals.s.id2ChildIds[nodeId] = message.data.children;
                    Tree.update(Globals.s.id2Node[nodeId]);
                    break;

                case 'loadCore':

                    message.data.forEach((element: any) => {
                        Globals.s.correctPath.push(element.nodeId);
                    });

                    for (let i = 0; i < message.data.length; i++) {

                        let element = message.data[i];

                        if (!Globals.s.id2Node[element.nodeId]) {

                            Globals.s.addNode(element.nodeId, element.parentId, element.label);
                            Globals.s.id2ChildIds[element.nodeId] = element.children;

                            element.children.forEach((kidId: any) => {

                                if (!Globals.s.correctPath.includes(kidId)) {

                                    Globals.s.addNode(kidId, element.nodeId, message.data[i + 1].label.replace("!", ""));
                                    Globals.loadChildIds(kidId);
                                }
                            });
                        }
                    }

                    Node.collapseNode(Globals.s.id2Node[Globals.s.rootId]);

                    Tree.update(Globals.s.id2Node[Globals.s.rootId]);

                    Globals.s.waiting = false;

                    Tree.selectNode(Globals.s.selectedId);

                    Listener.setLoadedCount();

                    break;

                case 'longestBranchingVariable':

                    Tree.tree.nodeSize([Number(message.data) * 13, 0]);
                    break;

                case 'loadNodes':

                    message.data.forEach((element: any) => {
                        if (!Globals.s.id2Node[element.nodeId]) {
                            Globals.s.addNode(element.nodeId, element.parentId, element.label);
                            Globals.s.id2ChildIds[element.nodeId] = element.children;
                        }
                    });

                    Tree.update(Globals.s.id2Node[Globals.s.rootId]);

                    Globals.s.waiting = false;

                    Tree.selectNode(Globals.s.selectedId);

                    Listener.setLoadedCount();

                    break;

                case 'simpleDomains':

                    Listview.updatePanelTitle();
                    Globals.s.currentDomainId += Number($("#domCount").val());

                    if (Globals.s.selectedId === Globals.s.rootId) {
                        Globals.s.simpleDomainsAtRoot = message.data.vars;

                        if (init) {
                            init = false;
                            Globals.s.waiting = false;
                            break;
                        }

                        $("#pane").empty();
                        Globals.tabulate();
                        Globals.appendRows(message.data.vars);
                    }

                    else {
                        message.data.vars.forEach((variable: any) => {
                            $("#" + $.escapeSelector(variable.name)).removeClass("changed");

                            let li = $("#" + $.escapeSelector(variable.name) + " > :last-child");
                            li.text(variable.rng);

                            if (message.data.changedNames.includes(variable.name)) {
                                $("#" + $.escapeSelector(variable.name)).toggleClass("changed");

                            }
                        });
                    }
                    Globals.s.waiting = false;

                    break;

                case 'prettyDomains':

                    Listview.updatePanelTitle();

                    if (Globals.s.selectedId === Globals.s.rootId) {
                        Listview.update(message.data);
                    }
                    else {
                        Listview.setChangedExpressions(message.data.changedExpressions);
                        Listview.updateNodes(message.data.vars);
                        Listview.setChangedList(message.data.changed);
                        Listview.setChanged();
                    }
                    Globals.s.waiting = false;
                    break;
            }
            Globals.s.waiting = false;
        });

    }
}
