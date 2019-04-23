// Algorithm A: addition of polynomials

var makePoly = function (a) {
  var last = {COEF:0,SIGN:-1,ABC:[0,0,0]};
  var ptr = last;
  var r = ptr;
  a.forEach(function (c) {
    var co = c[0];
    var x = c[1];
    var y = c[2];
    var z = c[3];
    if (co == 0) {
      return;
    }
    var n = {};
    n.COEF = co;
    n.SIGN = 1;
    n.ABC = [x,y,z];
    ptr.LINK = n;
    ptr = n;
  });
  ptr.LINK = r;
  return r;
};

var toNum = function (a) {
  return a[0]*100 + a[1]*10 + a[2];
};

var comp = function (a,b) {
  var aa = toNum(a);
  var bb = toNum(b);
  if (aa < bb) {
    return -1;
  }
  if (aa == bb) {
    return 1;
  }
  return 0;
}

var algo = function (P,Q) {
  var P;
  var LINK;
  var Q1;
  var Q2;
  var a1 = function () {
    P = P.LINK;
    Q1 = Q;
    Q = Q.LINK;
    return a2();
  }
  var a2 = function () {
    var j = comp(P.ABC,Q.ABC);
    if (j == -1) {
      Q1 = Q;
      Q = Q.LINK;
      return a2();
    }
    if (j == 0) {
      return a3();
    }
    return a5();
  }
  var a3 = function () {
    if (P.SIGN == -1) {
      return;
    }
    Q.COEF = Q.COEF + P.COEF;
    if (Q.COEF == 0) {
      return a4();
    }
    P = P.LINK;
    Q1 = Q;
    Q = Q.LINK;
    return a2();
  }
  var a4 = function () {
    Q2 = Q;
    Q1.LINK = Q = Q.LINK;
    P = P.LINK;
    return a2();
  }
  var a5 = function () {
    Q2 = {};
    Q2.COEF = P.COEF;
    Q2.ABC = P.ABC;
    Q2.LINK = Q;
    Q1.LINK = Q2;
    Q1 = Q2;
    P = P.LINK;
    return a2();
  }
  return a1();
}

var p = makePoly([[2,3,2,0],[4,1,2,1]]);
var q = makePoly([[4,3,2,0],[6,2,2,2],[1,0,1,0]]);

algo(p,q);
console.log(q);




