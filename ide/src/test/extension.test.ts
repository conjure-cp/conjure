//
// Note: This example test is leveraging the Mocha test framework.
// Please refer to their documentation on https://mochajs.org/ for help.
//

// The module 'assert' provides assertion methods from node
// import * as assert from 'assert';
import * as Parser from '../parser';
import fs = require('fs');
// import { parse } from 'url';

// You can import and use all API from the 'vscode' module
// as well as import your extension to test it
// import * as vscode from 'vscode';
// import * as myExtension from '../extension';

// Defines a Mocha test suite to group tests of similar kind together
suite("Extension Tests", function () {

        let testDir = "/home/tom/Documents/sh/essence/conjure-output";

        let eprime = (fs.readFileSync(testDir + "/model000001.eprime", 'utf8'));
        let minion = (fs.readFileSync(testDir + "/model000001.eprime-minion", 'utf8'));
        let json = (fs.readFileSync(testDir + "/out.json", 'utf8'));
        let path = "/test.db";

    test("Parse DB", () => {
        const sqlite3 = require('sqlite3').verbose();

        let db = new sqlite3.Database(path, (err: any) => {
            if (err) {
                console.error(err.message);
            }
            console.log('Connected to the database.');
        });

        console.log(db);

        // async function recursive(node: TreeNode) {
        //     const sql = 'select * from Nodes where ParentID=' + node.name;

        //     let rows = await new Promise<any>((res, rej) => {
        //         db.all(sql, [], (err: any, rows: any) => {
        //             if (err) {
        //                 rej(err);
        //             } else {
        //                 res(rows);
        //             }
        //         });
        //     });

        //     rows.forEach((row: any) => {
        //         node.children.push(new TreeNode(String(row.NodeID)));
        //     });

        //     await Promise.all(node.children.map((child) => recursive(child)));
        // }

        // let root = new TreeNode("0");
        // await recursive(root);
        // let parser = new Parser.DBParser(json, eprime, minion);
        // parser.parseDB(db);
        // console.log('        parser.parseDB();: ',         parser.parseDB(db));
        // let contents = parser;
        // console.log(contents);

    });

    test("Parse JSON", () => {
        let parser = new Parser.JSONParser(json, eprime, minion);
        let contents = parser.parseJson();
        console.log(contents);

    });

    // Defines a Mocha unit test
    // test("Something 1", function() {
    //     assert.equal(-1, [1, 2, 3].indexOf(5));
    //     assert.equal(-1, [1, 2, 3].indexOf(0));
    // });
});