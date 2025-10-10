program topogen
    use modmem
implicit none
real(kind=8),parameter::pi = 4*atan(1.0_8)
integer(kind=8),parameter::n=4096*2,m=2
!real(4)::dmat(n,n)
integer::f,f1,i,j,idx(n),k,l,k1
!logical::s1(n,n),s2(n,n,n)
real, parameter:: epsilon = .05
real(kind=8), allocatable::t(:,:), r(:,:), t2(:,:),ts1(:,:)
real(kind=4)::d,dmin

!s1=.false.
!s2=.false.
idx = 0
call allocm(t,n,m)
call allocm(r,n,m)
call allocm(ts1,m*2,n*(n-1)/2)
ts1=0.0_8
call random_number(t)
r(1:n/2,1)=(t(1:n/2,1)*.3+.85)*cos(2*pi*t(1:n/2,2))+1
r((n/2+1):,1)=(t((n/2+1):,1)*.3+.85)*cos(2*pi*t((n/2+1):,2))-1
r(1:n/2,2)=(t(1:n/2,1)*.3+.85)*sin(2*pi*t(1:n/2,2))
r((n/2+1):,2)=(t((n/2+1):,1)*.3+.85)*sin(2*pi*t((n/2+1):,2))
t2=transpose(r)
open(newunit=f,file='xy.txt')
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

!dmat=10*epsilon
do i = 1,n-1
  k=0
  f=0
  do j = i+1,n
    d = CABS(cmplx(r(i,1),r(i,2),4) - cmplx(r(j,1),r(j,2),4)) 
    
    if (d.lt.epsilon) then
      k=k+1
      if (j.gt.i+k) then 
        r(k+i:j:j-k-i,:)=r(j:k+i:k+i-j,:)
      end if
      !s1(i,i+k)=.true.
      k1=k1+1
      ts1(1,k1) = r(i,1)
      ts1(2,k1) = r(i,2)
      ts1(3,k1) = r(i+k,1)
      ts1(4,k1) = r(i+k,2)
      !dmat(i,i+k)=d
    end if

  end do
  if (k.gt.1) then
    do j = i+1, i+k-1
      do l = j+1,i+k
        d = CABS(cmplx(r(i,1),r(i,2),4) - cmplx(r(j,1),r(j,2),4)) 
        if (d.lt.epsilon) then
          !s2(i,j,l)=.true.
        end if
      end do
    end do
  end if
end do

call freem(t)

deallocate(t2)
open(newunit=f1,file='edges.txt',status='replace',action='write',iostat=f)
write (f1,'(4(1x,ES19.12))',err=501) ts1(:,1:k1)
501 close(f1)
call freem(r)
call freem(ts1)



end program