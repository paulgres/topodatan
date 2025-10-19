program hw2
    implicit none

    type :: point
      integer::num
    end type
    integer, parameter:: n=4,m=2
    logical::d1(n,n),d2(n,n),fa(n,n,n),ed(n,n)
    integer::i,j,k,tt(n),k1,k2,f,l,k3
    real:: eps,d,r,ts1(m*2,n*(n-1)/2),dvr1(n,n),dch1(n,n) , dvr2(n,n,n), dch2(n,n,n)
    real, dimension(2,4) ::pts
    pts = reshape((/ -1.,0.,1.,0.,0.,-2.,0.,2./), shape=(/m,n/))


    eps = 4
    write (*, '(2f6.2)') pts
k1=0
k2=0
k3=0
dvr1=-1.0
dch1=-1.0 
dvr2=-1.0
dch2=-1.0 
!dmat=10*epsilon
do i = 1,n-1
  k=0
  f=0
  do j = i+1,n
    if (dvr1(j,i).le.0.0) then
      d = anint(norm2(pts(:,i)-pts(:,j))*1000.0)/1000.0
      k2=k2+1
      k3=k3+1
      dvr1(j,i) = d
      dch1(j,i) = d/2.0
    else
      d=dvr1(j,i)
    end if
    if (d.le.eps) then
      k=k+1
      tt(k) = j
      k1=k1+1
      d1(k1,i) = .true.
      d1(k1,j) = .true.
      ts1(1:2,k1) = pts(:,i)
      ts1(3:4,k1) = pts(:,j)
    end if

  end do
  do j = i+1, n-1
      do l = j+1,n
        d = anint(norm2(pts(:,l)-pts(:,j))*1000.0)/1000.0
        dvr1(l,j)=d
        dch1(l,j)=d/2.0
        d = max(max(dvr1(i,j),dvr1(i,l)),d)
        dvr2(i,j,l)=d
        r=anint(trad(pts(:,i),pts(:,j),pts(:,l))*1000.0)/1000.0
        k3=k3+1
        
        dch2(i,j,l) = r
      end do
    end do
  
end do
print *, "k1 =  ", k1
print *, "k2 =  ", k2
print *, "k3 =  ", k3
print *
print *, "Viettoris-Rips"
write (*, '(4f6.2)') dvr1
print *
print *, "Cech"
write (*, '(4f6.2)') dch1
open(newunit=f,file='edges.txt',status='replace',action='write',iostat=f)
write (f,'(4(1x,ES19.12))',err=501) ts1(:,1:k1)
501 close(f)
print *, anint(trad(pts(:,1),pts(:,4),pts(:,3))*1000.0)/1000.0


contains
pure function trad(a,b,c)
  real, dimension(:),intent(in):: a,b,c
  real, allocatable, dimension(:)::v1,v2
  real :: trad
  real::alpha
  trad =0.0

  alpha = acos(dot_product(b-a,c-a)/norm2(b-a)/norm2(c-a))
  trad = norm2(b-a-(c-a))/2/sin(alpha)

end function

end program hw2