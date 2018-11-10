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

        let t: string = "UNKNOWN";

        if (this.type === Type.Int) {
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


abstract class SetVar extends Variable {
    public max: number = -1;
    public min: number = -1;
    protected excluded: number[] = [];
    protected included: number[] = [];
    public abstract getType(): String;
    public getIncluded() {
        return "int(" + this.included + ")";
    }

    public getExcluded() {
        return "int(" + this.excluded + ")";
    }
    public getCardinality() {
        let maxLength = this.max - this.excluded.length;
        let min = this.min + this.included.length;
        if (maxLength !== this.included.length) {
            return "int(" + min + ".." + maxLength + ")";
        }
        else {
            return "int(" + maxLength + ")";
        }
    }
    public include(n: number) {
        this.included.push(n);
    }

    public exclude(n: number) {
        this.excluded.push(n);
    }
}

class DummySet extends SetVar {

    constructor(v: Variable) {
        super(v.name, v.range, v.type);
    }
    public getType() {
        return "ExplicitVarSizeWithDummy";
    }
}

class OccurenceSet extends SetVar {

    constructor(v: Variable) {
        super(v.name, v.range, v.type);
    }

    public getType() {
        return "Occurence";
    }
}

enum Type {
    Int,
    Occurrence,
    ExplicitVarSizeWithDummy
}

export default class Parser {

    public setSeen: Type | undefined;
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
                obj.nodes.push({ "text": "Type", "nodes": [{ "text": set.getType() }] });
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
                let array = varObj[1].DomainSet;
                let d = array[2].DomainInt;
                let r = d[0];
                let b = r.RangeBounded;
                let lower = b[0];
                let upper = b[1];
                let min = lower.Constant.ConstantInt;
                let max = upper.Constant.ConstantInt;

                let set = undefined;

                if (array[0].Set_Occurrence) {
                    set = new OccurenceSet(variable);
                    this.setSeen = Type.Occurrence;
                }

                if (array[0].Set_ExplicitVarSizeWithDummy) {
                    set = new DummySet(variable);
                    this.setSeen = Type.ExplicitVarSizeWithDummy;
                }

                if (set) {
                    set.min = min;
                    set.max = max;
                    variable = set;
                }
                else {
                    throw new Error("Unsupported Set!");
                }
            }

            map[variable.name] = variable;
        }

        console.log(map);
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
                if (this.setSeen !== Type.ExplicitVarSizeWithDummy) {
                    variable.name = math.simplify(this.parseAuxOccurence(key)).toString();
                    map[variable.name] = new Expression(variable);
                }
            }
            else {
                variable.name = key;

                let split = variable.name.split("_");

                if (split.length > 1) {

                    let setName = split[0];

                    if (variable.range) {
                        let splits = variable.range.split("..");

                        if (setName in map) {

                            if (map[setName] instanceof DummySet) {
                                let set = <DummySet>map[setName];
                                if (splits.length === 1) {
                                    let discretePossibilities = variable.range.split(",");
                                    if (discretePossibilities.length === 1) {
                                        let num = Number(discretePossibilities[0].substring(1, discretePossibilities[0].length - 1));
                                        if (num === set.max + 1) {
                                            set.exclude(Number(split[split.length - 1]));
                                        }
                                        else {
                                            set.include(num);
                                        }
                                    }
                                }
                            }

                            if (map[setName] instanceof OccurenceSet) {

                                let set = <OccurenceSet>map[setName];
                                let number = Number(split[split.length - 1]);

                                if (splits.length === 1) {
                                    if (variable.range === "(0)") {
                                        set.exclude(number);
                                    }
                                    else {
                                        set.include(number);
                                    }
                                }
                                // map[setName] = set;
                            }
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

    public parseDomainsSimple(obj: any) {

        interface Domain {
            name: string;
            range: string;
        }

        let domainArray: Domain[] = [];
        let sorted = Object.keys(obj).sort();

        for (let i = 0; i < sorted.length; i++) {

            let entry: Domain = { "name": "", "range": "" };

            let key = sorted[i];

            for (let i = 0; i < obj[key].length; i++) {
                if (!(obj[key][i][0] === obj[key][i][1])) {
                    obj[key][i] = "(" + obj[key][i][0] + ".." + obj[key][i][1] + ")";
                }
            }

            entry.name = key;
            entry.range = Array.from(new Set(Parser.flattenArray(obj[key]))).toString();
            domainArray.push(entry);
        }

        return domainArray;
    }

    public parseTree(obj: any, treeviewDomainMap: any, normalDomainMap: any, simpleDomainMap: any) {

        obj["minionID"] = Number(obj["name"]);
        let domains = this.parseDomains(obj["Domains"]);
        normalDomainMap[obj["minionID"]] = domains;
        let simpleDomains = this.parseDomainsSimple(obj["Domains"]);
        simpleDomainMap[obj["minionID"]] = simpleDomains;
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
                this.parseTree(element, treeviewDomainMap, normalDomainMap, simpleDomainMap);
            });
        }
    }

    public parseJson() {

        const Stopwatch = require("node-stopwatch").Stopwatch;

        var stopwatch = Stopwatch.create();
        stopwatch.start();

        let obj = JSON.parse(this.jsonFile.toString());

        stopwatch.stop();

        console.log("JSON.PARSE: " + stopwatch.elapsedMilliseconds);

        stopwatch.reset();
        stopwatch.restart();

        obj = rename(obj, (key: any) => {
            if (key === 'Node') { return 'name'; }
            return key;
        });

        let treeviewDomainMap = {};
        let normalDomainMap = {};
        let simpleDomainMap = {};
        this.parseTree(obj, treeviewDomainMap, normalDomainMap, simpleDomainMap);
        // console.log(treeviewDomainMap);
        // console.log(normalDomainMap);

        stopwatch.stop();
        console.log("OTHER: " + stopwatch.elapsedMilliseconds);

        let result = {
            "tree": obj,
            "treeviewDomainMap": treeviewDomainMap,
            "normalDomainMap": normalDomainMap,
            "simpleDomainMap": simpleDomainMap
        };

        console.log(result);

        return result;
    }
}
