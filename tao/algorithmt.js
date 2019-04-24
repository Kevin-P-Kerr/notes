var ip = [[0,12],[2,3],[7,2],[11,1]];
var n = ip[0][1];
var Record  = function () {
    this.suc = undefined;
    this.next = undefined;
};

var t = function() {
    var COUNT = [];
    var TOP = [];
    var N;
    var rp = 0; // not in algo.
    var relation; // implicit
    var j,k; // implicit
    var P;
    var R;
    var QLINK = [];
    var F;
    var t1 = function () {
        var i = 1;
        for (;i<=n;i++) {
            COUNT[i] = 0;
            QLINK[i] = 0;
            TOP[i] = null;
        }
        N = n;
        return t2();
    };
    var t2 = function () {
        rp++;
        if (rp >= ip.length) {
            return t4();
        }
        relation = ip[rp];
        j = relation[0];
        k = relation[1];
        return t3();
    };
    var t3 = function () {
        COUNT[k] = COUNT[k]+1;
        P = new Record();
        P.suc = k;
        P.next = TOP[j];
        TOP[j] = P;
        return t2();
    };
    var t4 = function () {
        R = 0;
        QLINK[0] = 0;
        var i = 1;
        for (;i<=n;i++) {
            var c = COUNT[i];
            if (c == 0) {
                QLINK[R] = i;
                R = i;
            }
        }
        F = QLINK[0];
        return t5();
    };
    var t5 = function () {
        console.log(F);
        if (F == 0) {
            return t8();
        }
        N = N-1;
        P = TOP[F];
        return t6();
    };
    var t6 = function () {
        if (P == null) {
            return t7();
        }
        COUNT[P.suc] = COUNT[P.suc]-1;
        if (COUNT[P.suc] == 0) {
            QLINK[R] = P.suc;
            R = P.suc;
        }
        P = P.next;
        return t6();
    };
    var t7 = function () {
        F = QLINK[F];
        return t5();
    }
    var t8 = function () {
        if (N != 0) {
            console.log('***');
            console.log(N);
            console.log('error');
        }
    };
    return t1();
}


t();
