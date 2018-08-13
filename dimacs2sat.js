var fs = require('fs');

var d = fs.readFileSync('d.dimacs').toString();
var lines = d.split("\n");

var alpha = ['a','b','c','d','e','f','g','h','i','j','k','l','m'];
var ret = "";
lines.forEach(function (ln) {
    if (ln[0] == 'p' || ln[0] == 'c') {
        return;
    }
    ln = ln.split(' ');
    var first = true;
    ln.forEach(function (atom) {
        if (atom == '0') {
            return;
        }
        var na = "";
        if (first) {
            first = false;
        }
        else {
            na = "+";
        }
        if (atom[0] == '-') {
            na += "~";
            na += alpha[parseInt(atom.substr(1,atom.length-1),10)];
        }
        else {
            na+=alpha[parseInt(atom,10)];
        }
        ret += na;
    });
    ret += "*";
});

console.log(ret);

