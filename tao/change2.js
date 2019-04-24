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

var sum = function (a,b) {
    var r = {TAIL:true};
    var tail = r;
    a = a.LINK;
    b = b.LINK;
    while (!a.TAIL || !b.TAIL) {
        var node = {};
        if (a.TAIL) {
            node.BASE = b.BASE;
            node.COEF = b.COEF;
            b = b.LINK;
        }
        else if (b.TAIL) {
            node.BASE = a.BASE;
            node.COEF = a.COEF;
            a = a.LINK;
        }
        else if (a.BASE > b.BASE) {
            node.BASE = a.BASE;
            node.COEF = a.COEF;
            a = a.LINK;
        }
        else if (a.BASE == b.BASE) {
            node.BASE = a.BASE;
            node.COEF = a.COEF + b.COEF;
            a = a.LINK;
            b = b.LINK;
        }
        else {
            node.BASE = b.BASE;
            node.COEF = b.COEF;
            b = b.LINK;
        }
        r.LINK = node;
        r = node;
    }
    r.LINK = tail;
    return tail;
};

var toNum = function(p) {
    p = p.LINK;
    var r = 0;
    while (!p.TAIL) {
        r += (p.COEF * p.BASE);
    }
    return r;
};

var rebalance = function (p,a) {
    return findMaxForm(toNum(p),a);
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
// stash a copy of these as we will need them alot
var maxDenomForms = (function () {
    var r = {};
    var i = 0;
    var ii = a.length;
    var denom;
    for (;i<ii;i++) {
        r[a[i]] = findMaxForm(a[i], i == a.length-1? [1] : a.slice(i+1));
    }    
    return r;
})();

var findCombos = function (m) {
    var r = [];
    m = m.LINK;
    var current = m;
    while (!m.TAIL) {
        var c = m.COEF;
        var b = m.BASE;
        while (c > 0) {
            c--;
            var n = sum(current,maxDenomForms[b]);
            r.push(n);
            current = n;
        }
        m = m.LINK;
    }
    return r;
};

var makeChangeCombos = function (n) {
    var maxForm = findMaxForm(n,a);
    return findCombos(maxForm);
};

var z = makeChangeCombos(26);
z.forEach(function (l) { print(l); });


