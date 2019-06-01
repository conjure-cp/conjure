declare var acquireVsCodeApi: any;

import * as ConfigureHelper from "../../../configureHelper";

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

function collectFields(): ConfigureHelper.Configuration {

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
    };

    let config: ConfigureHelper.Configuration = {
        modelFileName: $('#modelFiles').find(":selected").text(),
        paramFileName: $('#paramFiles').find(":selected").text(),
        minionOptions: minionOptions,
        savileRowOptions: savileRowOptions,
        conjureOptions: conjureOptions
    };

    return config;


}

function toPlaceHolder(id: string) {
    if ($("#" + id).val()) {
        $("#" + id).attr("placeholder", $("#" + id).val() + "");
    }
    else {
        $("#" + id).attr("placeholder", "None");
    }
    $("#" + id).val("");

    console.log("fired");
}

$("#solve").click(() => {

    vscode.postMessage({
        command: "solve",
        data: collectFields()
    });

});


$("#diff").click(() => {
    let config1 = collectFields();
    toPlaceHolder("nodelimit");
    toPlaceHolder("sollimit");
    toPlaceHolder("cpuLimit");
    toPlaceHolder("timeLimit");
    toPlaceHolder("cnfLimit");
    toPlaceHolder("conjureTimeLimit");
});


vscode.postMessage({
    command: "init"
});