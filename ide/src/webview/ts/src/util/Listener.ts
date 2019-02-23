import Listview from './Listview';
import Globals from './Globals';
import Tree from './Tree';
import Node from './Node';

export default class Listener {

    public static setLoadedCount() {
        $("#total").text(Globals.s.totalLoaded + "/" + "?");
    }

    public static bindListener() {

        window.addEventListener('message', event => {
            const message = event.data;


            switch (message.command) {

                case 'loadSet':
                    Globals.lv.id2Node[message.data.structure.name].children = message.data.structure.children;
                    Globals.lv.updateFromRoot();
                    Globals.lv.updateNodes([message.data.update]);
                    Globals.sendPrettyRequest();
                    break;

                case 'init':
                    // console.log(message.data.simple);
                    Globals.lv.update(message.data.pretty);
                    Globals.s.simpleDomainsAtRoot = message.data.simple.vars;
                    break;

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

                    Tree.selectNode(Globals.s.selectedId);

                    Listener.setLoadedCount();

                    break;

                case 'simpleDomains':

                    Globals.lv.updatePanelTitle();
                    message.data.vars.forEach((variable: any) => {
                        $("#" + $.escapeSelector(variable.name)).removeClass("changed");

                        let li = $("#" + $.escapeSelector(variable.name) + " > :last-child");
                        li.text(variable.rng);

                        if (message.data.changedNames.includes(variable.name)) {
                            $("#" + $.escapeSelector(variable.name)).toggleClass("changed");
                        }
                    });
                    break;

                case 'prettyDomains':

                    Globals.lv.updatePanelTitle();
                    Globals.lv.setChangedExpressions(message.data.changedExpressions);
                    Globals.lv.updateNodes(message.data.vars);
                    Globals.lv.setChanged(message.data.changed);
                    break;
            }
            Globals.s.waiting = false;
        });

    }
}
