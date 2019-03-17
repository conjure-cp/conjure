import State from './State';
import Panel from './Listview';

declare var acquireVsCodeApi: any;

/**
 * This class is used to interact with the vscode api to send messages to the extension
 */
export default class Globals {
    public static lv = new Panel();
    public static vscode = acquireVsCodeApi();

    /**
     * Requests to load the child nodes of the selected node.
     * @param vscodeApi  
     */
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

    /**
     * Sends the appropriate request to load the domains`
     * @param vscodeApi 
     */
    public static loadDomains(vscodeApi: any) {
        if (State.pretty) {
            Globals.sendPrettyRequest(vscodeApi);
        }
        else {
            Globals.sendSimpleRequest(vscodeApi);
        }
    }

    /**
     * Sends a request for simple domains.
     * @param vscodeApi 
     */
    public static sendSimpleRequest(vscodeApi: any) {
        vscodeApi.postMessage({
            command: "simpleDomains",
            nodeId: State.selectedId,
            wantExpressions: !$("#expressions").prop("checked"),
        });
    }

    /**
     * Sends a request for pretty domains.
     * @param vscodeApi 
     */
    public static sendPrettyRequest(vscodeApi: any) {
        vscodeApi.postMessage({
            command: "prettyDomains",
            nodeId: State.selectedId,
            wantExpressions: !$("#expressions").prop("checked"),
            paths: State.pathList.join(":")
        });
    }

    /**
     * Sends requests on initialisation
     * @param vscodeApi 
     */
    public static initialize(vscodeApi: any) {
        vscodeApi.postMessage({
            command: 'init',
        });

        vscodeApi.postMessage({
            command: 'longestBranchingVariable',
        });
    }
}