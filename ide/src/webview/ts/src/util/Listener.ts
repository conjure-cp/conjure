import Listview from './Listview';
import Globals from './Globals';
import Tree from './Tree';

export default class Listener {

    public static bindListener() {

        let init = true;

        window.addEventListener('message', event => {
            const message = event.data
            switch (message.command) {
                case 'loadChildren':
                    Globals.data.id2ChildIds[message.data.nodeId] = message.data.children
                    Tree.update(Globals.data.id2Node[message.data.nodeId]);

                    break;
                case 'loadCore':

                    console.log(message.data);

                    message.data.forEach((element: any) => {
                        Globals.data.correctPath.push(element.nodeId);
                    });

                    for (let i = 0; i < message.data.length; i++) {

                        let element = message.data[i]

                        if (!Globals.data.id2Node[element.nodeId]) {

                            Globals.data.addNode(element.nodeId, element.parentId, element.label);
                            Globals.data.id2ChildIds[element.nodeId] = element.children

                            element.children.forEach((kidId: any) => {

                                if (!Globals.data.correctPath.includes(kidId)) {

                                    Globals.data.addNode(kidId, element.nodeId, message.data[i + 1].label.replace("!", ""))
                                    Globals.vscode.postMessage({
                                        command: 'loadChildren',
                                        id: kidId,
                                    });
                                }

                            })
                        }
                    }

                    Globals.data.collapseNode(Globals.data.rootId);

                    // console.log(Globals.data.id2Node);

                    Tree.update(Globals.data.id2Node[Globals.data.rootId]);
                    Globals.data.waiting = false;

                    Tree.selectNode(Globals.data.selectedId);

                    $("#total").text(Globals.data.totalLoaded + "/" + Globals.data.correctPath[Globals.data.correctPath.length - 1]);

                    break;

                case 'longestBranchingVariable':
                    Tree.tree.nodeSize([Number(message.data) * 13, 0])

                    break;

                case 'loadNodes':

                    message.data.forEach((element: any) => {

                        if (!Globals.data.id2Node[element.nodeId]) {
                            Globals.data.addNode(element.nodeId, element.parentId, element.label);
                            Globals.data.id2ChildIds[element.nodeId] = element.children
                        }
                    });

                    Tree.update(Globals.data.id2Node[0]);
                    Globals.data.waiting = false;

                    Tree.selectNode(Globals.data.selectedId);

                    $("#total").text(Globals.data.totalLoaded + "/" + Globals.data.correctPath[Globals.data.correctPath.length - 1]);

                    break;

                case 'simpleDomains':

                    Listview.setNodeId();
                    Globals.data.currentDomainId += Number($("#domCount").val());

                    if (Globals.data.selectedId === Globals.data.rootId) {
                        Globals.data.simpleDomainsAtRoot = message.data.vars;

                        if (init) {
                            init = false;
                            Globals.data.waiting = false;
                            break;
                        }

                        $("#pane").empty();
                        Globals.tabulate()
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
                    Globals.data.waiting = false;

                    break;

                case 'prettyDomains':

                    Listview.setNodeId();

                    if (Globals.data.selectedId === Globals.data.rootId) {
                        Listview.render(message.data, message.data);
                    }
                    else {
                        Listview.setChangedExpressions(message.data.changedExpressions);
                        Listview.updateNodes(message.data.vars);
                        Listview.setChangedList(message.data.changed);
                        Listview.setChanged();
                    }
                    Globals.data.waiting = false;
                    break;
            }
            Globals.data.waiting = false;
        });

    }
}
