program simcomp
    use modmem
    use modgauss
implicit none
real(kind=8),parameter::pi = 4*atan(1.0_8)

!real(4)::dmat(n,n)
integer::f,f1,i,j,k,l,k1,k2,n,m,l1,tt(1024)
logical, allocatable::d1(:,:),d2(:,:),t(:,:)!,s2(:,:,:)
real, parameter:: epsilon = .45
real(kind=8), allocatable::r(:,:), ts1(:,:)
real(kind=4)::d,stop,start
character(50)::fmt1, fmt2
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


call allocb(d1,n*(n-1)/2,n)
d1=.false.
!s1=.false.
!s2=.false.


call allocm(ts1,m*2,n*(n-1)/2)
ts1=0.0_8

k1=0
k2=0

!dmat=10*epsilon
do i = 1,n-1
  k=0
  f=0
  do j = i+1,n
    d = CABS(cmplx(r(1,i),r(2,i),4) - cmplx(r(1,j),r(2,j),4)) 
    
    if (d.lt.epsilon) then
      k=k+1
      tt(k) = j
      !if (j.gt.i+k) then 
      !  r(:,k+i:j:j-k-i)=r(:,j:k+i:k+i-j)
      !end if
      
      !s1(i,i+k)=.true.
      k1=k1+1
      !if (j.eq.1024) then
      !  print *,i,j,k1
      !end if
      d1(k1,i) = .true.
      d1(k1,j) = .true.
      ts1(1:2,k1) = r(:,i)
      ts1(3:4,k1) = r(:,j)
      !dmat(i,i+k)=d
    end if

  end do
  if (k.gt.1) then
    do j = 1, k-1
      do l = j+1,k
        d = CABS(cmplx(r(1,l),r(2,l),4) - cmplx(r(1,j),r(2,j),4)) 
        
        if (d.lt.epsilon) then
          k2=k2+1
          !d2(k2,)
          !s2(i,j,l)=.true.
        end if
      end do
    end do
  end if
end do
print *, "k1 =  ", k1
print *, "k2 =  ", k2
open(newunit=f1,file='edges.txt',status='replace',action='write',iostat=f)
write (f1,'(4(1x,ES19.12))',err=501) ts1(:,1:k1)
501 close(f1)


!##########################################################################
!########################## Gauss #########################################
!##########################################################################
call allocb(t,k1,n)
t = d1(1:k1,:)
call freeb(d1)
fmt1 = '('//int2str(n)//'l1)'
open(newunit=f1,file='d1.txt',status='replace',action='write',iostat=f)
write (f1,fmt1,err=505) transpose(t)
505 close(f1)
call cpu_time(start)
!t(61,:)=.true.
l=triangl(t)

call cpu_time(stop)
print *, "rank: ",l
    print *, 'elapsed:', stop-start
open(newunit=f1,file='d1t.txt',status='replace',action='write',iostat=f)
write (f1,fmt1,err=505) transpose(t)
506 close(f1)
!##########################################################################




call freeb(d1)
call freem(r)
call freem(ts1)



end program simcomp