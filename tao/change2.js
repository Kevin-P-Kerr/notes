
var findMinForm = function (n,a) {
    var tail = {TAIL:true};
    var node = {};
    var b = a[a.length-1]; // assume a[n+1] < a[n]
    if (b != 1 || b > n || b%n != 0) {
      throw new Error();
    }
    node.COEF = b%n;
    node.BASE = b;
    node.RIGHT = tail;
    tail.LEFT = node;
    var i = 0;
    var ii = a.length-1;
    var head = tail;
    for (;i<ii;i++) {
      node = {};
      node.BASE = a[i];
      node.COEF = 0;
      node.LEFT = head;
      head.RIGHT = node;
      head = node;
    }
    return tail;
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

var makeChangeCombos = function (n) {
    var maxForm = findMaxForm(n,a);
    return findCombos(maxForm);
};

var z = makeChangeCombos(30);
z.forEach(function (l) { print(l); });


