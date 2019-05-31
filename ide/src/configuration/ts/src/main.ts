declare var acquireVsCodeApi: any;

import * as ConfigureHelper from "../../../configureHelper";
import ConjureHelper from "../../../conjureHelper";

const vscode = acquireVsCodeApi();

window.addEventListener('message', event => {

    const message = event.data;

    switch (message.command) {
        case 'files':

            console.log(message.data);

            $.each(message.data.essenceFiles, (_index: number, fileName: string) => {
                $("#modelFiles").append($("<option></option>")
                    .attr("value", "1").text(fileName));
            });
            $.each(message.data.paramFiles, (_index: number, fileName: string) => {
                $("#paramFiles").append($("<option></option>")
                    .attr("value", "1").text(fileName));
            });
            break;
    }
});

$("#solve").click(() => {

    let selectedModel = $('#modelFiles').find(":selected").text();
    let selectedParam = $('#paramFiles').find(":selected").text();

    let minionOptions: ConfigureHelper.MinionOptions;
    minionOptions = { findallsols: $("#findallsols").is(":checked"), nodelimit: Number($("#nodelimit")) };

    let savileRowOptions: ConfigureHelper.SavileRowOptions;
    savileRowOptions = { optimisation: $('#optimisation').find(":selected").text() };

    let payload: ConfigureHelper.Configuration = {
        modelFileName: selectedModel,
        paramFileName: selectedParam,
        minionOptions: minionOptions,
        savileRowOptions: savileRowOptions
    };

    vscode.postMessage({
        command: "solve",
        data: payload
    });

})



vscode.postMessage({
    command: "init"
});