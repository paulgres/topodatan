program simcomp
    use modmem
implicit none
real(kind=8),parameter::pi = 4*atan(1.0_8)

!real(4)::dmat(n,n)
integer::f,f1,i,j,k,l,k1,n,m
!logical::s1(n,n),s2(n,n,n)
real, parameter:: epsilon = .2
real(kind=8), allocatable::r(:,:), ts1(:,:)
real(kind=4)::d
open(newunit=f, file='xy.txt', status='old', action='read')
n = 0
m=2
do
  read(unit=f, fmt=*, end=11)
  n = n + 1
end do
11  continue
call allocm(r,m,n)
rewind(f)
read (f,'(2(1x,ES19.12))',err=502) r
502 close(unit=f)

!s1=.false.
!s2=.false.


call allocm(ts1,m*2,n*(n-1)/2)
ts1=0.0_8



!dmat=10*epsilon
do i = 1,n-1
  k=0
  f=0
  do j = i+1,n
    d = CABS(cmplx(r(1,i),r(2,i),4) - cmplx(r(1,j),r(2,j),4)) 
    
    if (d.lt.epsilon) then
      k=k+1
      if (j.gt.i+k) then 
        r(:,k+i:j:j-k-i)=r(:,j:k+i:k+i-j)
      end if
      !s1(i,i+k)=.true.
      k1=k1+1
      ts1(1:2,k1) = r(:,i)
      ts1(3:4,k1) = r(:,i+k)
      !dmat(i,i+k)=d
    end if

  end do
  if (k.gt.1) then
    do j = i+1, i+k-1
      do l = j+1,i+k
        d = CABS(cmplx(r(1,i),r(2,i),4) - cmplx(r(1,j),r(2,j),4)) 
        
        if (d.lt.epsilon) then
          !s2(i,j,l)=.true.
        end if
      end do
    end do
  end if
end do




open(newunit=f1,file='edges.txt',status='replace',action='write',iostat=f)
write (f1,'(4(1x,ES19.12))',err=501) ts1(:,1:k1)
501 close(f1)
call freem(r)
call freem(ts1)
end program simcomp