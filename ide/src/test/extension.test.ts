//
// Note: This example test is leveraging the Mocha test framework.
// Please refer to their documentation on https://mochajs.org/ for help.
//

// The module 'assert' provides assertion methods from node
import * as assert from 'assert';
import * as chai from 'chai';
import fs = require('fs');
import { parse } from 'url';

// You can import and use all API from the 'vscode' module
// as well as import your extension to test it
import * as vscode from 'vscode';
import * as request from "request-promise-native";

import * as init from '../server/init';

let serverURL = "http://localhost:3000/";
let testDataPath = "/home/tom/SearchTreeVisualisationTests/testData/";

suite("Error cases", function () {

    test("Non-existant path", async () => {
        chai.expect(() => init.findFiles("/sad/ad/as/")).to.throw("ENOENT");
    });

    test("No DB file", async () => {
        chai.expect(() => init.findFiles(testDataPath + "extension/noDBFile")).to.throw("db");
    });

    test("No eprime file", async () => {
        chai.expect(() => init.findFiles(testDataPath + "extension/noEprimeFile")).to.throw("eprime");
    });

    test("No minion file", async () => {
        chai.expect(() => init.findFiles(testDataPath + "extension/noMinionFile")).to.throw("minion");
    });

    test("multiple db file", async () => {
        chai.expect(() => init.findFiles(testDataPath + "extension/multipleDBFiles")).to.throw("db");
    });

    test("multiple eprime file", async () => {
        chai.expect(() => init.findFiles(testDataPath + "extension/multipleEprimeFiles")).to.throw("eprime");
    });

    test("multiple minion file", async () => {
        chai.expect(() => init.findFiles(testDataPath + "extension/multipleMinionFiles")).to.throw("minion");
    });

});

suite("Error cases", function () {
    test("golomb", async () => {
        let res = (init.findFiles(testDataPath + "golomb"));
        chai.expect(res.db).to.contain("out.db");
        chai.expect(res.eprime).to.contain("model000001.eprime");
        chai.expect(res.minion).to.contain("model000001-05.eprime-minion");
    });

});