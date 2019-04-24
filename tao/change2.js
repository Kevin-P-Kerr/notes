var findMaxForm = function (n,a) {
    var i = 0;
    var ii = a.length;
    var v;
    var q,r;
    var qr;
    var tail = {TAIL:true};
    var current = tail;
    var node;
    for (;i<ii;i++) {
        v = a[i];
        if (v > n) {
            continue;
        }
        qr = findQR(n,v);
        q = qr[0];
        r = qr[1];
        node = {};
        node.COEF = q;
        node.BASE = v;
        n = r;
        current.LINK = node;
        current = node;
    }
    current.LINK = tail;
    return tail;
};

var findQR = function (x,d) {
    var q = Math.floor(x/d);
    var r = x-(d*q);
    return [q,r];
};

var print = function (p) {
    p = p.LINK;
    var str = "";
    while (!p.TAIL) {
        str += p.COEF+"("+p.BASE+") + ";
        p = p.LINK;
    }
    str += '0';
    console.log(str);
};

var a = [25,10,5,1];
var f = findMaxForm(33,a);
print(f);


