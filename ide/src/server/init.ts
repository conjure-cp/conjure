import fs = require('fs');
import { FilesParam } from 'typescript-rest';

interface InitFiles {
    db : string;
    eprime : string;
    minion : string;
}

function makeMessage(extension: string, count: number): string {
    return "Expected 1 " + extension + " file, got: " + count;  

}

export function findFiles(path: string): InitFiles {

    let files = fs.readdirSync(path);

    let eprimeFiles = files.filter(el => /\.eprime$/.test(el));

    if (eprimeFiles.length !== 1) {
        throw new Error(makeMessage("eprime", eprimeFiles.length));
    }

    let dbFiles = files.filter(el => /\.db$/.test(el));

    if (dbFiles.length !== 1) {
        throw new Error(makeMessage("db", dbFiles.length));
    }

    let minionFiles = files.filter(el => /\.eprime-minion$/.test(el));

    if (minionFiles.length !== 1) {
        throw new Error(makeMessage("minion", minionFiles.length));
    }

    return {db: dbFiles[0], eprime: eprimeFiles[0], minion: minionFiles[0]};

}