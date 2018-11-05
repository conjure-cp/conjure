import fs = require('fs');
import * as math from 'mathjs';
let rename = require('deep-rename-keys');
let empty = require('is-empty');
// let jsondiffpatch = require('jsondiffpatch').create();
// let d3 = require("d3");


class TreeNode {

    public size: number;
    public children: TreeNode[] = [];

    constructor(public name: string) {
        this.size = 6000;
    }
}

class Variable {
    public name: string;
    public range: string | undefined;
    public type: Type | undefined;

    constructor(name?: string, range?: string, type?: Type) {
        if (name) {
            this.name = name;
        }
        else {
            this.name = "name not set yet";
        }
        this.range = range;
        this.type = type;
    }

    public getPrettyRange() {

        let t : string = "UNKNOWN";

        if (this.type === Type.Int){
            t = "int";
        }

        if (this.range) {
            return t + this.range;
        }
    }
}

class Expression extends Variable {
    constructor(v: Variable) {
        super(v.name, v.range, Type.Int);
    }
}

class SetVar extends Variable {

    public table: any | undefined;
    private excluded: number[] = [];
    private included: number[] = [];


    // constructor(name : string, range : string, type : string, size : number){
    constructor(v: Variable) {
        super(v.name, v.range, v.type);
        this.table = {};
    }

    private getProperties() {

        this.excluded = [];
        this.included = [];

        for (const key in this.table) {
            switch (this.table[key]) {
                case Status.Excluded:
                    this.excluded.push(Number(key));
                    break;
                case Status.Included:
                    this.included.push(Number(key));
                    break;
                default:
                    break;
            }
        }
    }

    public getIncluded() {
        this.getProperties();
        return "int(" + this.included + ")";
    }

    public getExcluded() {
        this.getProperties();
        return "int(" + this.excluded + ")";
    }

    public getCardinality(): any {
        this.getProperties();
        let maxLength = (Object.keys(this.table).length - this.excluded.length);
        if (maxLength !== this.included.length) {
            return "int(" + this.included.length + ".." + maxLength + ")";
        }
        else {
            return "int(" + maxLength + ")";
        }
    }
}

enum Status {
    Included,
    Excluded,
    Unknown
}

enum Type {
    Int,
    Occurrence,
    ExplicitVarSizeWithDummy
}

export default class Parser {

    // public parseJson(jsonFile: string, esenceFile: string, eprimeFile: string, minionFile: string, ) {
    public auxMap: any;
    public jsonFile: string;
    public essenceFile: string | undefined;
    public eprimeFile: string;
    public minionFile: string;
    public jsonAux = new RegExp('aux[0-9]+');

    constructor(jsonFile: string, eprimeFile: string, minionFile: string) {
        this.jsonFile = jsonFile;
        // this.essenceFile = essenceFile;
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


    public domainsToHierachy(values: any) {

        let varList: any[] = [];
        let setList: any[] = [];
        let expressionList: any[] = [];

        values.forEach((element: Variable) => {
            let obj: any = {};
            obj.nodes = [];
            if (element instanceof SetVar) {
                let set = <SetVar>element;
                obj.text = set.name;
                obj.nodes.push({ "text": "Type", "nodes": [{ "text": set.type }] });
                obj.nodes.push({ "text": "Cardinality", "nodes": [{ "text": set.getCardinality() }] });
                obj.nodes.push({ "text": "Excluded", "nodes": [{ "text": set.getExcluded() }] });
                obj.nodes.push({ "text": "Included", "nodes": [{ "text": set.getIncluded() }] });
                setList.push(obj);
            }
            else {
                obj.text = element.name;
                obj.nodes.push({ "text": element.getPrettyRange() });
                if (element instanceof Expression) {
                    expressionList.push(obj);
                }
                else {
                    varList.push(obj);
                }
            }

        });

        return [{
            "text": "Items", "nodes": [{ "text": "Expressions", "nodes": expressionList },
            { "text": "Variables", "nodes": varList }, { "text": "Sets", "nodes": setList }]
        }];

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

        let map: any = {};

        let lines: string[] = this.eprimeFile.split("\n");

        for (let i = 0, len = lines.length; i < len; i++) {
            lines[i] = lines[i].substr(2);
        }

        let uncommented = (lines.join(''));
        let representations = JSON.parse(uncommented.split("Conjure's")[1]).representations;

        for (const key in representations) {
            let variable: Variable = new Variable();
            let varObj = representations[key];
            variable.name = varObj[0].Name;
            if (varObj[1].DomainInt) {
                variable.type = Type.Int;
            }
            if (varObj[1].DomainSet) {
                variable = new SetVar(variable);
                let array = varObj[1].DomainSet;
                if (array[0].Set_Occurrence) {
                    variable.type = Type.Occurrence;
                }
                if (array[0].Set_ExplicitVarSizeWithDummy) {
                    variable.type = Type.ExplicitVarSizeWithDummy;
                }
            }

            map[variable.name] = variable;
        }

        // console.log(map);
        return map;
    }

    public parseAuxOccurence(key: string) {
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

    public parseAuxDummy(key: string) {
    }

    public parseDomains(obj: any) {

        let map = this.parseEprime();

        let sorted = Object.keys(obj).sort();

        for (let i = 0; i < sorted.length; i++) {

            let key = sorted[i];
            let variable = new Variable();

            for (let i = 0; i < obj[key].length; i++) {
                if (!(obj[key][i][0] === obj[key][i][1])) {
                    variable.range = "(" + obj[key][i][0] + ".." + obj[key][i][1] + ")";
                }
                else {
                    variable.range = "(" + Array.from(new Set(Parser.flattenArray(obj[key]))).toString() + ")";
                }
            }

            if (this.jsonAux.test(key)) {
                variable.name = math.simplify(this.parseAuxOccurence(key)).toString();
                map[variable.name] = new Expression(variable);
            }
            else {
                variable.name = key;

                let split = variable.name.split("_");

                if (split.length > 1) {

                    let setName = split[0];

                    if (setName in map) {
                        let set = <SetVar>map[setName];
                        let number = split[split.length - 1];

                        if (variable.range) {

                            let splits = variable.range.split("..");

                            if (splits.length === 1) {
                                // console.log(variable.range);
                                if (variable.range === "(0)") {
                                    set.table[number] = Status.Excluded;
                                }
                                else {
                                    set.table[number] = Status.Included;
                                }
                            }
                            else {
                                set.table[number] = Status.Unknown;
                            }
                            map[setName] = set;
                        }
                    }
                }
                else {
                    variable.type = map[variable.name].type;
                    map[variable.name] = variable;
                }
            }

        }
        return map;
    }

    public parseTree(obj: any, treeviewDomainMap: any, normalDomainMap: any) {

        obj["minionID"] = Number(obj["name"]);
        let domains = this.parseDomains(obj["Domains"]);
        // console.log(domains);
        normalDomainMap[obj["minionID"]] = domains;
        delete obj.Domains;
        let treeViewDomains = this.domainsToHierachy(Object.values(domains));
        treeviewDomainMap[obj["minionID"]] = treeViewDomains;

        let splitted = obj['branchVar'].split("_");
        if (splitted.length > 1) {
            if (splitted[0] in domains) {
                let num = Number(splitted[splitted.length - 1]);
                if (obj['branchVal'] === 1) {
                    obj["name"] = "Include " + num + " in " + splitted[0];
                }
                else {
                    obj["name"] = "Exclude " + num + " from " + splitted[0];
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
                this.parseTree(element, treeviewDomainMap, normalDomainMap);
            });
        }
    }

    public parseJson() {
        let obj = JSON.parse(this.jsonFile.toString());

        obj = rename(obj, (key: any) => {
            if (key === 'Node') { return 'name'; }
            return key;
        });

        let treeviewDomainMap = {};
        let normalDomainMap = {};
        this.parseTree(obj, treeviewDomainMap, normalDomainMap);
        console.log(treeviewDomainMap);
        // console.log(normalDomainMap);

        return {
            "tree": obj,
            "treeviewDomainMap": treeviewDomainMap,
            "normalDomainMap": normalDomainMap
        };
    }
}
