const sqlite3 = require('sqlite3').verbose();

let db = new sqlite3.Database("/home/tom/minion-private/build/gears/test.db");
let query = "select nodeId, parentId from Node where parentId >= 0;"

let count = {0:0};
let d = {};
async function blah() {

    let rows = await new Promise((res, rej) => {
        db.all(query, [], (err, rows) => {
            if (err) {
                rej(err);
            } else {
                res(rows);
            }
        });
    });

    await rows.forEach((row) => {
        d[Number(row.nodeId)] = Number(row.parentId);
        count[Number(row.nodeId)] = 0;
    });

    
    await Object.keys(d).forEach((nodeId) => {
        while (1) {
            count[nodeId]+=1;
            if (d[nodeId] === undefined) {
                break;
            }
            else {
                nodeId = d[nodeId];
            }
        }
    });
    console.log(count[0]);
}

blah();


