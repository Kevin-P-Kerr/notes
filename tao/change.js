// version A: interview version with caching
var makeChangeA = (function (denom) {
  var c = {0:0} // make the cache, plus a sanity check
  var last = denom.length-1;
  var find = function (n) {
    if (!isNaN(c[n])) {
      return c[n];
    }
    var r = 0;
    var i = 0;
    var ii = denom.length;
    var currentCoin;
    var subn;
    for (;i<ii;i++) {
      if (i == last) {
        r += 1;
        break;
      }
      currentCoin = denom[i];
      subn = n-currentCoin;
      if (subn < 0) {
        continue;
      }
      r +=  find(subn);
    }
    c[n] = r;
    return r;
  };
  return find;
})([25,10,5,1]);

var i = 0, ii =101;
for (;i<ii;i++) {
  console.log(i,makeChangeA(i));
}



  
