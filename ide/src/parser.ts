import fs = require('fs');
import * as math from 'mathjs';
let rename = require('deep-rename-keys');
let empty = require('is-empty');


class TreeNode {

    public size: number;
    public children: TreeNode[] = [];

    constructor(public name: string) {
        this.size = 6000;
    }
}

export default class Parser {

    public static async parseDB(path: string): Promise<TreeNode> {
        const sqlite3 = require('sqlite3').verbose();

        let db = new sqlite3.Database(path, (err: any) => {
            if (err) {
                console.error(err.message);
            }
            console.log('Connected to the database.');
        });

        async function recursive(node: TreeNode) {
            const sql = 'select * from Nodes where ParentID=' + node.name;

            let rows = await new Promise<any>((res, rej) => {
                db.all(sql, [], (err: any, rows: any) => {
                    if (err) {
                        rej(err);
                    } else {
                        res(rows);
                    }
                });
            });

            rows.forEach((row: any) => {
                node.children.push(new TreeNode(String(row.NodeID)));
            });

            await Promise.all(node.children.map((child) => recursive(child)));
        }

        let root = new TreeNode("0");
        await recursive(root);

        let newJson = JSON.stringify(root);
        fs.writeFileSync('cpp.json', newJson, 'utf8');
        console.log(root);
        // console.log("HERE");

        db.close((err: any) => {
            if (err) {
                console.error(err.message);
            }
            console.log('Close the database connection.');
        });


        return root;
    }

    public static parseJson(jsonFile: string, esenceFile: string, eprimeFile: string, minionFile: string, ) {

        let obj = JSON.parse(jsonFile.toString());

        obj = rename(obj, (key: any) => {
            if (key === 'Node') { return 'name'; }
            return key;
        });

        function flatten(arr: any) {
            return arr.reduce((flat: any, toFlatten: any) => {
                return flat.concat(Array.isArray(toFlatten) ? flatten(toFlatten) : toFlatten);
            }, []);
        }


        // function expandAuxExpression(str: string, key: string) {
        //     let minoinAux = new RegExp(key +  ' #(.*)');
        //     if (minoinAux) {
        //         minionFile.forEach(element => {

        //         });
        //     }

        // }

        function parseDomains(obj: any) {

            interface Domain {
                name: string;
                range: string;
            }

            let domainArray: Domain[] = [];
            let dict: any = {};
            let sorted = Object.keys(obj).sort();

            for (let i = 0; i < sorted.length; i++){

                let entry: Domain = { "name": "", "range": "" };

                let key = sorted[i];

                for(let i = 0; i < obj[key].length; i++){
                    if (!(obj[key][i][0] === obj[key][i][1])) {
                        obj[key][i] = "(" + obj[key][i][0] + ".." + obj[key][i][1] + ")";
                    }
                }

                let intermediate : string = key; 
                let jsonAux = new RegExp('aux[0-9]+');

                if (jsonAux.test(key)) {
                    let minoinAux = new RegExp(key + ' #(.*)');
                    let match = minoinAux.exec(minionFile);
                    if (match) {
                        intermediate = match[1];
                        let newMatches = jsonAux.exec(intermediate);
                        if (newMatches) {
                            for (let i = 0; i < newMatches.length; i++) {
                                if (newMatches[i] in dict) {
                                    intermediate = intermediate.replace(newMatches[i], dict[newMatches[i]]);
                                }
                            }
                        }
                        else {
                            dict[key] = intermediate;
                        }
                    }
                    entry.name = math.simplify(intermediate).toString();
                    console.log(entry.name);
                }
                else{
                    entry.name = key;
                }

                entry.range = Array.from(new Set(flatten(obj[key]))).toString();
                domainArray.push(entry);
            }


            // console.log(domainArray);
            return domainArray;
        }

        function parseTree(obj: any) {

            for (const prop in obj) {
                if (prop === "name") {
                    obj["minionID"] = String(obj[prop]);
                    if (!(obj[prop] === "")) {
                        obj[prop] = obj['branchVar'] + " set to: " + obj['branchVal'];
                    }
                }

                if (prop === 'Domains') {
                    obj[prop] = parseDomains(obj[prop]);
                }
            }

            obj.children = [];

            if (obj.left && !empty(obj.left)) {
                obj.children.push(obj.left);
            }

            if (obj.right && !empty(obj.right)) {
                obj.children.push(obj.right);

            }

            delete obj.right;
            delete obj.left;

            if (obj.children.length === 0) {
                delete obj.children;
            }

            else {
                obj.children.forEach((element: any) => {
                    parseTree(element);
                });
            }
        }

        parseTree(obj);
        // console.log(obj);
        return obj;
    }
}
