program topogen
    use modmem
implicit none
real(kind=8),parameter::pi = 4*atan(1.0_8)
integer(kind=8),parameter::n=5000,m=2
integer::f
real(kind=8), allocatable::t(:,:), r(:,:), t2(:,:)
call allocm(t,n,m)
call allocm(r,n,m)
call random_number(t)

r(1:n/2,1)=(t(1:n/2,1)*.3+.85)*cos(2*pi*t(1:n/2,2))+1
r((n/2+1):,1)=(t((n/2+1):,1)*.3+.85)*cos(2*pi*t((n/2+1):,2))-1
r(1:n/2,2)=(t(1:n/2,1)*.3+.85)*sin(2*pi*t(1:n/2,2))
r((n/2+1):,2)=(t((n/2+1):,1)*.3+.85)*sin(2*pi*t((n/2+1):,2))
t2=transpose(r)
open(newunit=f,file='xy.txt')
write (f,'(2(1x,ES19.12))',err=500) t2
500 close(f)
deallocate(t2)
call freem(t)
call freem(r)
end program