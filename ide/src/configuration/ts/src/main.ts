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

function getNumberOrZero(id: string) {
    let nl = Number($("#" + id).val());
    if (isNaN(nl)) {
        nl = 0;
    }
    return nl;
}

$("#solve").click(() => {


    let minionOptions: ConfigureHelper.MinionOptions;

    minionOptions = {
        findallsols: $("#findallsols").is(":checked"),
        randomiseorder: $("#randomiseorder").is(":checked"),
        nodelimit: getNumberOrZero("nodelimit"),
        sollimit: getNumberOrZero("sollimit"),
        cpulimit: getNumberOrZero("cpulimit"),
        preprocessing: $("#preprocessing").find(":selected").text(),
        consistency: $("#consistency").find(":selected").text()
    };

    console.log(minionOptions);

    let savileRowOptions: ConfigureHelper.SavileRowOptions;
    savileRowOptions = {
        optimisation: $('#optimisation').find(":selected").text(),
        symmetryBreaking: $('#symmetryBreaking').find(":selected").text(),
        translation: $('#translation').find(":selected").text(),
        timelimit: getNumberOrZero("timeLimit"),
        cnflimit: getNumberOrZero("cnfLimit")
    };

    let conjureOptions: ConfigureHelper.ConjureOptions;

    conjureOptions = {
        timelimit: getNumberOrZero("conjureTimeLimit"),
        strategy: String($('#strategy').find(":selected").val())
    }

    let payload: ConfigureHelper.Configuration = {
        modelFileName: $('#modelFiles').find(":selected").text(),
        paramFileName: $('#paramFiles').find(":selected").text(),
        minionOptions: minionOptions,
        savileRowOptions: savileRowOptions,
        conjureOptions: conjureOptions
    };

    vscode.postMessage({
        command: "solve",
        data: payload
    });

});



vscode.postMessage({
    command: "init"
});