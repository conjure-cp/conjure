import State from './State';
import Listview from '../util/Listview';

declare var acquireVsCodeApi: any;

export default class Globals {
    public static lv = new Listview();
    public static vscode = acquireVsCodeApi();
    public static columns = ["Name", "Domain"];


    public static loadNodes(vscodeApi: any) {
        if (!State.waiting) {
            vscodeApi.postMessage({
                command: 'loadNodes',
                amount: 1,
                start: State.selectedId
            });

            State.waiting = true;
        }
    }

    public static loadDomains(vscodeApi: any) {
        if (State.pretty) {
            Globals.sendPrettyRequest(vscodeApi);
        }
        else {
            Globals.sendSimpleRequest(vscodeApi);
        }
    }

    public static sendSimpleRequest(vscodeApi: any) {
        vscodeApi.postMessage({
            command: "simpleDomains",
            nodeId: State.selectedId,
            wantExpressions: !$("#expressions").prop("checked"),
        });
    }

    public static sendPrettyRequest(vscodeApi: any) {
        vscodeApi.postMessage({
            command: "prettyDomains",
            nodeId: State.selectedId,
            wantExpressions: !$("#expressions").prop("checked"),
            paths: State.pathList.join(":")
        });
    }

    public static initialize(vscodeApi: any) {
        vscodeApi.postMessage({
            command: 'init',
        });

        vscodeApi.postMessage({
            command: 'longestBranchingVariable',
        });
    }
}