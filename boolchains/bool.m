x[k_]:=(2^32-1)/(2^(2^(5-k))+1)
x1=x[1]
x2=x[2]
x3=x[3]
x4=x[4]
x5=x[5]
c[t_]:=2^32-1-t
cx1=c[x1]
cx2=c[x2]
cx3=c[x3]
cx4=c[x4]
cx5=c[x5]
b[t_]:=BaseForm[t,16]
ba=BitAnd
bo=BitOr
bx=BitXor
