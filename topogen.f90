program topogen
    use modmem
    use modaux
implicit none
real(kind=4),parameter::pi = 4*atan(1.0)
integer(kind=4),parameter::n=512,m=2
!real(4)::dmat(n,n)
integer::f,i,j,idx(n),k


real(kind=4), allocatable::t(:,:), r(:,:), t2(:,:), pts(:,:)
real(kind=4)::d,dmin,c
c = cos(pi/4.)
    !pts = reshape((/ -1.,0.,0.,-2.,1.,0.,0.,2.0/), shape=(/m,n/))
    !pts = reshape((/-c,0.,0.,c,c,0.,0.,-c,3.+c,0. /), shape=(/m,n/))
idx = 0
call allocm(t,n,m)
call allocm(r,n,m)
!call allocm(pts,n,m)
!pts = reshape((/ -1.,0.,0.,-2.,1.,0.,0.,2.0/), shape=(/m,n/))

    !pts = reshape((/-c,0.,0.,c,c,0.,0.,-c,3.+c,0. /), shape=(/m,n/))
call random_number(t)
k=n/10*6
t((k+1):,2) = t((k+1):,2)*5./6.+1./12.
r(1:k,1)=(t(1:k,1)*.3+.85)*cos(2*pi*t(1:k,2))+1
r((k+1):,1)=(t((k+1):,1)*.3+.85)*cos(2*pi*t((k+1):,2))-1
r(1:k,2)=(t(1:k,1)*.3+.85)*sin(2*pi*t(1:k,2))
r((k+1):,2)=(t((k+1):,1)*.3+.85)*sin(2*pi*t((k+1):,2))
t2=transpose(r)
open(newunit=f,file='xy.txt')
write (f,'(2(1x,ES19.12))',err=502) t2
502 close(f)
open(newunit=f,file='xyv.txt')
do i = 1,n
  dmin = 5.0_8
  do j = 0,11
    d = CABS(cmplx(r(i,1),r(i,2),4) - cmplx(cos(j*pi/6)+1,sin(j*pi/6),4))
    if (d.lt.dmin) then
        idx(i) = j+1
        dmin = d
    end if
    if (j.gt.0)then
      d = CABS(cmplx(r(i,1),r(i,2),4) - cmplx(cos(j*pi/6)-1,sin(j*pi/6),4))
      if (d.lt.dmin) then
        idx(i) = j+13
        dmin = d
      end if
    end if
  end do
write (f,'(2(1x,ES19.12), I3)',err=500) t2(1,i),t2(2,i), idx(i)
end do
500 close(f)


call freem(t)

deallocate(t2)
call freem(pts)
call freem(r)




end program