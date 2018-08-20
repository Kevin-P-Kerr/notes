var fs = require('fs');

var d = fs.readFileSync('factoring.dimacs').toString();
var lines = d.split("\n");

var alpha = ['a','b','c','d','e','f','g','h','i','j','k','l','m'];
var ret = "";
var f = true;
lines.forEach(function (ln) {
    if (ln[0] == 'p' || ln[0] == 'c') {
        return;
    }
    ln = ln.split(' ');
    var first = true;
    var intermediate = "";
    ln.forEach(function (atom) {
        if (atom == '0' || !atom) {
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
            na +=  atom.substr(1,atom.length-1);
        }
        else {
            na+=atom;
        }
        intermediate += na;
    });
    if (!intermediate) {
        return;
    }
    if (f) { 
        f = false;
        ret += intermediate;
    }
    else {
        ret = (ret+ "*"+intermediate); 
    }
});

var n = 0;
for (;n<10;n++) {
    ret = ret.replace(new RegExp(n+"", 'g'),alpha[n]);
}

console.log(ret);

