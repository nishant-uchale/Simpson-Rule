
program num_int
implicit none
real::exact,trap_value,simp_value,trap_diff,simp_diff,h,term1,term2,term3,sum_odd,sum_even,a,b,x,y 
integer::n,i,j
open (15, file='out.dat')
a=0
b=3.1415
exact=((exp(3.0*b))/3.0)+(b**2.0)-(1.0/3.0) 
write(15,*)" Exact value =",exact
write(15,*)" Sub-int  Trapezoidal_value   Trapezoidal_diff   Simpson_value    Simpson_diff"
write(15,*)"------------------------------------------------------------------------------"         
do n=2,60,2
h=(b-a)/n
term1=(Fun(a)+Fun(b))
sum_odd=0
i=1
201 sum_odd=sum_odd+(Fun(a+i*h))
i=i+2
x=n-1
if(i.le.x)goto 201
term2=sum_odd
sum_even=0
j=2
301 sum_even=sum_even+(Fun(a+j*h))
j=j+2
y=n-2
if(j.le.y)goto 301
term3=sum_even
trap_value=((term1)*h/2.0)+(term2+term3)*h
trap_diff=trap_value-exact
simp_value=(term1+(term2*4.0)+(term3*2.0))*h/3
simp_diff=simp_value-exact
101 format(3x,i2,9x,f10.4,9x,f10.4,8x,f10.4,8x,f10.4)
write(15,101)n,trap_value,trap_diff,simp_value,simp_diff      			  											   
end do
contains
function Fun(x)
real::Fun,x
Fun=exp(3*x)+2*x
end function Fun
end program num_int
