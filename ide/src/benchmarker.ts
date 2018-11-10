import fs = require('fs');
var fileName = process.argv.slice(2)[0];
let parsed = JSON.parse(fs.readFileSync(fileName, 'utf8'));