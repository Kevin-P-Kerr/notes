
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
    str = str.trim();
    str = str.slice(0,str.length-1);
    console.log(str);
};

var propagate = function (m) {
  var tail = m;
  m = m.RIGHT.RIGHT;
  var l = m.LEFT;
  while (!m.TAIL) {
    if (l.TAIL) { 
      return false;
    }
    var n = m.COEF*m.BASE;
    if (n >= l.BASE) {
      n = n-l.BASE;
      m.COEF = Math.floor(n/m.BASE);
      n = n - m.COEF;
      if (n > 0) {
        tail.LEFT.COEF += n;
      }
      l.COEF += 1;
      return true;
    }
    m = m.RIGHT;
    l = l.RIGHT;
  }
  return false;
};

var a = [25,10,5,1];

var findCombos = function (mf) {
  var r = [copy(mf)];
  while (propagate(mf)) {
    r.push(copy(mf));
  }
  return r;
};

var makeChangeCombos = function (n,a) {
  var minForm = findMinForm(n,a);
  return findCombos(minForm);
};

var z = makeChangeCombos(100,a);
console.log(z.length);
z.forEach(function (l) { print(l); });


