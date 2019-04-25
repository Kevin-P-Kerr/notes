
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

var findMaxForm = function (n,a) {
  var tail = {TAIL:true};
  var node;
  var head = tail;
  var i = 0;
  var ii = a.length;
  for (;i<ii;i++) {
    node = {};
    var b = a[i]; // assume a[i] > a[i+1]
    node.BASE = b;
    node.COEF = Math.floor(n/b);
    n = n - (node.BASE*node.COEF);
    node.LEFT = head;
    head.RIGHT = node;
    head = node;
  }
  tail.LEFT = head;
  head.RIGHT = tail;
  return tail;
};

var toNum = function (p) {
  p = p.LEFT;
  r = 0;
  while (!p.TAIL) {
    r += (p.COEF*p.BASE);
    p = p.LEFT;
  }
  return r;
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

var collectRightBases = function (m) {
  var r = [];
  while (!m.TAIL) {
    r.push(m.BASE);
    m = m.RIGHT;
  }
  return r;
};

var propagate = function (m) {
  var tail = m;
  m = m.LEFT.LEFT;
  while (!m.TAIL) {
    if (m.COEF == 0) {
      m = m.LEFT;
      continue;
    }
    var bases = collectRightBases(m.RIGHT);
    if (bases.length == 0) {
      return;
    }
    m.COEF = m.COEF-1;
    var z = tail.RIGHT;
    tail.RIGHT = m.RIGHT;
    m.RIGHT.LEFT = tail;
    var n = toNum(tail) + m.BASE;
    var r = findMaxForm(n,bases);
    m.RIGHT = r.RIGHT;
    r.RIGHT.LEFT = m;
    z.LEFT = r;
    r.RIGHT = z;
    
    print(r);
    console.log('**');
    return r;
  }
  return false;
};

var a = [25,10,5,1];

var findCombos = function (mf) {
  var r = [copy(mf)];
  mf = propagate(mf);
  while (mf) {
    r.push(copy(mf));
    mf = propagate(mf);
  }
  return r;
};

var makeChangeCombos = function (n,a) {
  var maxForm = findMaxForm(n,a);
  return findCombos(maxForm);
};

var z = makeChangeCombos(100,a);
console.log(z.length);
z.forEach(function (l) { print(l); });


