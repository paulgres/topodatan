


a = [0;2];
b = [-1;0];
c = [0;-2];
b1 = [1;0];

trad(a,b,c)
trad(a,b,b1)
%%



d= [1.5;0.];
l1=norm(a-d,2)
l2=norm(b-d,2)
l3=norm(c-d,2)
d1 = [0.;.75]
l1=norm(a-d1,2)
l2=norm(b-d1,2)
l3=norm(b1-d1,2)
function trad(a,b,c)
v1 = c-b;
v2 = a-b;
alpha = acos(dot(v1,v2)/norm(v1,2)/norm(v2,2));
l = norm(v2-v1,2);
trad= l/(2*sin(alpha))
end


