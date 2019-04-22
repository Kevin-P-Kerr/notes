var ip = [[0,9],[3,4],[4,2],[5,9],[9,3],[0,0]];
var n = ip[0][1];
var Record  = function () {
    this.suc = undefined;
    this.next = undefined;
};

var t = function() {
    var COUNT = [];
    var TOP = [];
    var N;
    var rp = 1; // not in algo.
    var relation; // implicit
    var j,k; // implicit
    var P;
    var R;
    var QLINK = [];
    var F;
    var t1 = function () {
        var i = 1;
        for (;i<n;i++) {
            COUNT[i] = 0;
            TOP[i] = null;
        }
        N = n;
        return t2();
    };
    var t2 = function () {
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
        p.suc = k;
        p.next = TOP[j];
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
        COUNT[p.suc] = COUNT[p.suc]-1;
        if (COUNT[p.suc] == 0) {
            QLINK[R] = p.suc;
            R = p.suc;
        }
        p = p.ext;
        return t6();
    };
    var t7 = function () {
        F = QLINK[F];
        return t5();
    }
    var t8 = function () {
        if (N != 0) {
            console.log('error');
        }
    };
    return t1();
}


debugger;
t();






        

