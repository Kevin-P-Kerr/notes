
var findMinForm = function (n,a) {
    var tail = {TAIL:true};
    var node = {};
    var b = a[a.length-1]; // assume a[n+1] < a[n]
    if (b != 1 && (b > n || b%n != 0)) {
      throw new Error();
    }
    node.COEF = n/b;
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
    head.RIGHT = tail.LEFT;
    tail.LEFT.LEFT = head;
    return tail;
};

var copy = function (p) {
  p = p.RIGHT;
  var r = {TAIL:true};
  var h = r;
  var node;
  while (!p.TAIL) {
    node = {};
    node.COEF = p.COEF;
    node.BASE = p.BASE;
    node.LEFT = h
    h.RIGHT = node;
    h = node;
    p = p.RIGHT;
  }
  h.RIGHT = r;
  r.LEFT = h;
  return r;
};

var print = function (p) {
    p = p.RIGHT;
    var str = "";
    while (!p.TAIL) {
        str += p.COEF+"("+p.BASE+") + ";
        p = p.RIGHT;
    }
    console.log(str);
};

var a = [25,10,5,1];

var findCombos = function (mf) {
  var r = [copy(mf)];
  return r;

};

var makeChangeCombos = function (n,a) {
  var minForm = findMinForm(n,a);
  print(minForm);
  return findCombos(minForm);
};

var z = makeChangeCombos(30,a);
z.forEach(function (l) { print(l); });


