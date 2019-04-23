// Algorithm A: addition of polynomials

var makePoly = function (a) {
  var last = {COEF:0,SIGN:-1};
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
  }
  return r;
};

var subComp = function (a,b) {
  if (a > b) {
    return 1;
  }
  if (b > a) {
    return -1;
  }
  return 0;
};

var comp = function (a,b) {
  var r = subComp(a[0],b[0]);
  if (r != 0) { return r; };
  var r = subComp(a[1],b[1]);
  if (r != 0) { return r; };
  var r = subComp(a[1],b[1]);
  if (r != 0) { return r; };
  return 1;
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
  }


