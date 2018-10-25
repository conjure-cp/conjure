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

class Variable {
    public name: string | undefined;
    public range: string | undefined;
    public type: string | undefined;

    constructor(name?: string, range?: string, type?: string) {
        this.name = name;
        this.range = range;
        this.type = type;
    }
}

class Expression extends Variable{
    constructor(v: Variable){
        super(v.name, v.range, v.type);
    }
}

class SetVar extends Variable {

    public table: any | undefined;
    public rejects: number[] = [];
    public chosen: number[] = [];
    public size: number | undefined;


    // constructor(name : string, range : string, type : string, size : number){
    constructor(v: Variable) {
        super(v.name, v.range, v.type);
        this.table = {};
    }

    public getProperties() {
        for (const key in this.table) {
            switch (this.table[key]) {
                case Status.Rejected:
                    this.rejects.push(Number(key));
                    break;
                case Status.Accepted:
                    this.chosen.push(Number(key));
                    break;
                default:
                    break;
            }
        }

        this.size = Object.keys(this.table).length - this.rejects.length;
    }
}

enum Status {
    Accepted,
    Rejected,
    Unknown
}


export default class Parser {

    // public parseJson(jsonFile: string, esenceFile: string, eprimeFile: string, minionFile: string, ) {
    public auxMap: any;
    public jsonFile: string;
    public essenceFile: string;
    public eprimeFile: string;
    public minionFile: string;
    public jsonAux = new RegExp('aux[0-9]+');

    constructor(jsonFile: string, essenceFile: string, eprimeFile: string, minionFile: string) {
        this.jsonFile = jsonFile;
        this.essenceFile = essenceFile;
        this.eprimeFile = eprimeFile;
        this.minionFile = minionFile;
        this.auxMap = {};
    }

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


    public static flattenArray(arr: any) {
        return arr.reduce((flat: any, toFlatten: any) => {
            return flat.concat(Array.isArray(toFlatten) ? Parser.flattenArray(toFlatten) : toFlatten);
        }, []);
    }

    public static flattenObject(data: any) {

        var result: any = {};
        function recurse(cur: any, prop: any) {
            if (Object(cur) !== cur) {
                result[prop] = cur;
            } else if (Array.isArray(cur)) {
                for (var i = 0, l = cur.length; i < l; i++) {
                    recurse(cur[i], prop + "[" + i + "]");
                }
                if (l === 0) {
                    result[prop] = [];
                }
            } else {
                var isEmpty = true;
                for (var p in cur) {
                    isEmpty = false;
                    recurse(cur[p], prop ? prop + "." + p : p);
                }
                if (isEmpty && prop) {
                    result[prop] = {};
                }
            }
        }
        recurse(data, "");
        return result;
    }

    public parseEprime() {

        // let domainArray: Variable[] = [];
        let map: any = {};

        let lines: string[] = this.eprimeFile.split("\n");

        for (let i = 0, len = lines.length; i < len; i++) {
            lines[i] = lines[i].substr(2);
        }

        let uncommented = (lines.join(''));
        let representations = JSON.parse(uncommented.split("Conjure's")[1]).representations;

        let flat = (Parser.flattenObject(representations));
        let variable: Variable = new Variable();

        let nameSeen = false;

        for (const key in flat) {

            if (key.includes("Name")) {

                if (nameSeen) {
                    if (variable.name) {
                        map[variable.name] = variable;
                    }
                    variable = new Variable();
                    nameSeen = false;
                }
                variable.name = flat[key];
                nameSeen = true;
            }

            if (key.includes("DomainInt")) {
                variable.type = "int";
            }

            if (key.includes("DomainSet")) {
                if (key.includes("Set_Occurrence")) {
                    variable = new SetVar(variable);
                    variable.type = "Occurrence";
                    if (variable.name) {
                        map[variable.name] = variable;
                    }
                    variable = new Variable();
                }
            }

        }
        return map;
    }

    public parseAux(key: string) {
        let minoinAux = new RegExp(key + ' #(.*)');
        let match = minoinAux.exec(this.minionFile);
        let intermediate: string = key;
        if (match) {
            intermediate = match[1];
            let newMatches = this.jsonAux.exec(intermediate);
            if (newMatches) {
                for (let i = 0; i < newMatches.length; i++) {
                    if (newMatches[i] in this.auxMap) {
                        intermediate = intermediate.replace(newMatches[i], this.auxMap[newMatches[i]]);
                    }
                }
            }
            else {
                this.auxMap[key] = intermediate;
            }
        }

        return intermediate;
    }

    public domainsToHierachy(values: any) {

        let varList: any[] = [];
        let setList: any[] = [];
        let expressionList: any[] = [];

        values.forEach((element: Variable) => {
            let obj: any = {};
            obj.nodes = [];
            if (element instanceof SetVar) {
                let set = <SetVar>element;
                // console.log(set);
                obj.text = set.name;
                set.getProperties();
                obj.nodes.push({ "text": "Type", "nodes": [{ "text": set.type }] });
                obj.nodes.push({ "text": "Cardinality", "nodes": [{ "text": set.size }] });
                obj.nodes.push({ "text": "Rejected", "nodes": [{ "text": set.rejects.toString() }] });
                obj.nodes.push({ "text": "Chosen", "nodes": [{ "text": set.chosen.toString() }] });
                setList.push(obj);
            }
            else {
                obj.text = element.name;
                obj.nodes.push({ "text": element.range});
                if (element instanceof Expression){
                    expressionList.push(obj);
                }
                else{
                    varList.push(obj);
                }
            }

        });


        return [{"text" : "Expressions", "nodes": expressionList},
         {"text": "Variables", "nodes" : varList}, {"text" : "Sets", "nodes" : setList}];


    }

    public parseDomains(obj: any) {

        let map = this.parseEprime();

        let sorted = Object.keys(obj).sort();

        for (let i = 0; i < sorted.length; i++) {

            let variable = new Variable();

            let key = sorted[i];

            for (let i = 0; i < obj[key].length; i++) {
                if (!(obj[key][i][0] === obj[key][i][1])) {
                    obj[key][i] = "(" + obj[key][i][0] + ".." + obj[key][i][1] + ")";
                }
            }
            console.log(obj);


            if (this.jsonAux.test(key)) {
                variable.name = math.simplify(this.parseAux(key)).toString();
                variable.range = Array.from(new Set(Parser.flattenArray(obj[key]))).toString();
                map[variable.name] = new Expression(variable);
            }
            else {
                variable.name = key;
                variable.range = Array.from(new Set(Parser.flattenArray(obj[key]))).toString();

                let split = variable.name.split("_");

                if (split.length > 1) {

                    let setName = split[0];

                    if (setName in map) {
                        let set = <SetVar>map[setName];
                        let number = split[split.length - 1];

                        let splits = variable.range.split("..");

                        if (splits.length === 1) {
                            if (variable.range === "0") {
                                set.table[number] = Status.Rejected;
                            }
                            else {
                                set.table[number] = Status.Accepted;
                            }
                        }
                        else {
                            set.table[number] = Status.Unknown;
                        }
                        map[setName] = set;
                    }
                }
                else {
                    map[variable.name] = variable;
                }
            }
        }
        return map;
    }

    public parseTree(obj: any) {

        obj["minionID"] = String(obj["name"]);
        let domains = this.parseDomains(obj["Domains"]);

        obj["Domains"] = this.domainsToHierachy(Object.values(domains));

        let splitted = obj['branchVar'].split("_");
        if (splitted.length > 1) {
            if (splitted[0] in domains) {
                let num = Number(splitted[splitted.length - 1]);
                if (obj['branchVal'] === "1") {
                    obj["name"] = "Accept " + num + " in " + splitted[0];
                }
                else {
                    obj["name"] = "Reject " + num + " from " + splitted[0];
                }

            }

        }
        else {
            obj["name"] = "Set " + obj['branchVar'] + " to " + obj['branchVal'];

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
                this.parseTree(element);
            });
        }
    }

    public parseJson() {
        let obj = JSON.parse(this.jsonFile.toString());
        obj = rename(obj, (key: any) => {
            if (key === 'Node') { return 'name'; }
            return key;
        });

        this.parseTree(obj);
        // console.log(obj);
        return obj;
    }
}
