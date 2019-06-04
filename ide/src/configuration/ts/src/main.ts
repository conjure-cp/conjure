declare var acquireVsCodeApi: any;

import * as ConfigureHelper from "../../../configureHelper";

const vscode = acquireVsCodeApi();

window.addEventListener('message', event => {

    const message = event.data;

    switch (message.command) {
        case 'files':

            // console.log(message.data);

            $.each(message.data.essenceFiles, (_index: number, fileName: string) => {
                $("#modelfiles").append($("<option></option>")
                    .attr("value", fileName).text(fileName));
            });
            $.each(message.data.paramFiles, (_index: number, fileName: string) => {
                $("#paramfiles").append($("<option></option>")
                    .attr("value", fileName).text(fileName));
            });

            config1 = collectFields();
            config2 = collectFields();
            break;
    }
});

function getNumberOrUndefined(id: string): number | undefined {
    let nl = Number($("#" + id).val());
    if (isNaN(nl) || nl === 0) {
        return undefined;
    }
    return nl;
}

function collectFields(): ConfigureHelper.Configuration {

    let minionOptions: ConfigureHelper.MinionOptions;

    minionOptions = {
        findallsols: $("#findallsols").is(":checked"),
        randomiseorder: $("#randomiseorder").is(":checked"),
        nodelimit: getNumberOrUndefined("nodelimit"),
        sollimit: getNumberOrUndefined("sollimit"),
        cpulimit: getNumberOrUndefined("cpulimit"),
        preprocessing: $("#preprocessing").find(":selected").text(),
        consistency: $("#consistency").find(":selected").text()
    };

    let savileRowOptions: ConfigureHelper.SavileRowOptions;
    savileRowOptions = {
        optimisation: $('#optimisation').find(":selected").text(),
        symmetryBreaking: $('#symmetrybreaking').find(":selected").text(),
        translation: $('#translation').find(":selected").text(),
        timelimit: getNumberOrUndefined("timelimit"),
        cnflimit: getNumberOrUndefined("cnflimit")
    };

    let conjureOptions: ConfigureHelper.ConjureOptions;

    conjureOptions = {
        timelimit: getNumberOrUndefined("conjuretimelimit"),
        strategy: String($('#strategy').find(":selected").val())
    };

    let config: ConfigureHelper.Configuration = {
        modelFileName: $('#modelfiles').find(":selected").text(),
        paramFileName: $('#paramfiles').find(":selected").text(),
        minionOptions: minionOptions,
        savileRowOptions: savileRowOptions,
        conjureOptions: conjureOptions
    };

    return config;
}

function setFields(config: ConfigureHelper.Configuration) {

    $("#findallsols").prop('checked', config.minionOptions.findallsols);
    $("#randomiseorder").prop('checked', config.minionOptions.randomiseorder);
    $("#nodelimit").val(config.minionOptions.nodelimit!);
    $("#sollimit").val(config.minionOptions.sollimit!);
    $("#cpulimit").val(config.minionOptions.cpulimit!);
    $("#preprocessing").val(config.minionOptions.preprocessing);
    $("#consistency").val(config.minionOptions.consistency);

    $("#optimisation").val(config.savileRowOptions.optimisation);
    $("#symmetrybreaking").val(config.savileRowOptions.symmetryBreaking);
    $("#translation").val(config.savileRowOptions.translation);
    $("#timelimit").val(config.savileRowOptions.timelimit!);
    $("#cnflimit").val(config.savileRowOptions.cnflimit!);

    $("#strategy").val(config.conjureOptions.strategy!);
    $("#conjuretimelimit").val(config.conjureOptions.timelimit!);

    $("#modelfiles").val(config.modelFileName);
    $("#paramfiles").val(config.paramFileName);
}

// function setSelectionBoxes(config: ConfigureHelper.Configuration){
// }

function toPlaceHolder(id: string) {
    if ($("#" + id).val()) {
        $("#" + id).attr("placeholder", $("#" + id).val() + "");
    }
    else {
        $("#" + id).attr("placeholder", "None");
    }
    $("#" + id).val("");
}

function resetPlaceHolders() {
    $(".tbox").attr("placeholder", "None");
}


let config1: ConfigureHelper.Configuration;
let config2: ConfigureHelper.Configuration;

$("#solve").click(() => {

    let configs: ConfigureHelper.Configuration[] = [];

    let diffing = $("#diff").is(":checked");

    if (!diffing) {
        configs = [collectFields()];
    }
    else {
        if ($("#2Label").hasClass("active")) {
            config2 = collectFields();
        }
        else {
            config1 = collectFields();
        }
        configs = [config1, config2];
    }



    vscode.postMessage({
        command: "solve",
        data: { configs: configs },
        diff: diffing
    });

});

$(".rButton").change(function () {
    $("#1Label").removeClass("active");
    $("#2Label").removeClass("active");
    switch ($(this).val()) {
        case '1':
            resetPlaceHolders();
            $("#1Label").addClass("active");
            config2 = collectFields();
            setFields(config1);
            // console.log("button 1");
            // console.log(config2.modelFileName);
            break;
        case '2':
            $("#2Label").addClass("active");
            config1 = collectFields();
            toPlaceHolder("nodelimit");
            toPlaceHolder("sollimit");
            toPlaceHolder("cpulimit");
            toPlaceHolder("timelimit");
            toPlaceHolder("cnflimit");
            toPlaceHolder("conjuretimelimit");
            setFields(config2);
            // console.log("button 2");
            // console.log(config2.modelFileName);
            break;
    }
});

$("#diff").click(() => {
    if ($("#diff").is(":checked")) {
        $('#1Label').removeClass('disabled');
        $('#2Label').removeClass('disabled');
    }
    else {
        $('#1Label').addClass('disabled');
        $('#2Label').addClass('disabled');
    }
});

$('#1Label').addClass('disabled');
$('#2Label').addClass('disabled');

vscode.postMessage({
    command: "init"
});