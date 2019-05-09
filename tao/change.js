// version A: interview version with caching
var makeChangeA = (function (denom) {
  var c = {0:0} // make the cache, plus a sanity check
  var last = denom.length-1;
  var find = function (n,suppressed) {
    if (!isNaN(c[n])) {
      //return c[n]; // no caching for now
    }
    var r = 0;
    var i = 0;
    var ii = denom.length;
    var currentCoin;
    var subn;
    var save = n;
    for (;i<ii;i++) {
      if (i <= denom.indexOf(suppressed)) {
        continue;
      }
      if (i == last) {
        r += 1;
        break;
      }
      currentCoin = denom[i];
      while (n > 0) {
        n = n-currentCoin;
        if (n < 0) {
          break;
        }
        if (n == 0) {
            r+=1;
            break;
        }
        r +=  find(n,currentCoin);
      }
      n = save;
    }
    c[n] = r;
    return r;
  };
  return find;
})([25,10,5,1]);

var i = 0, ii =201;
for (;i<ii;i++) {
  console.log(i,makeChangeA(i));
}



  
