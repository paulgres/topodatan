program hw2
  use modgauss
    implicit none

    type :: point
      integer::num
    end type
    integer, parameter:: n=4,m=2
    logical::d1(n*(n-1)/2,n),d2(n,n),fa(n,n,n),ed(n,n)
    integer::i,j,k,tt(n),k1,k2,f,l,k3
    real:: eps,d,r,edges(m*2+2,n*(n-1)/2),dvr1(n,n),dch1(n,n) , dvr2(n,n,n), dch2(n,n,n), dch(n*(n-1)),dvr(n*(n-1)/2)
    real, dimension(2,4) ::pts
    pts = reshape((/ 0.,-2.,-1.,0.,1.,0.,0.,2./), shape=(/m,n/))


    eps = 4
    write (*, '(2f6.2)') pts
k1=0
k2=0
k3=0
dvr1=-1.0
dch1=-1.0 
dvr2=-1.0
dch2=-1.0 
d1=.false.
!dmat=10*epsilon
do i = 1,n-1
  k=0
  f=0
  do j = i+1,n
    if (dvr1(j,i).le.0.0) then
      d = nint((norm2(pts(:,i)-pts(:,j))+.001)*1000.0)/1000.0
      k2=k2+1
      k3=k3+1
      dvr1(j,i) = d
      dvr(k2)=d
      dch1(j,i) = d/2.0
      dch(k3)=d/2.0
    else
      d=dvr1(j,i)
    end if
    
      
      k1=k1+1
      d1(k1,i) = .true.
      d1(k1,j) = .true.
      edges(1:2,k1) = pts(:,i)
      edges(3:4,k1) = pts(:,j)
      edges(5,k1) = d
      edges(6,k1) = d/2.0
    

  end do
  do j = i+1, n-1
      do l = j+1,n
        d = round(norm2(pts(:,l)-pts(:,j))+.001,3)
        !dvr1(l,j)=d
        !dch1(l,j)=d/2.0
        !k3=k3+1
        
        
        !dch(k3)=d/2.0
        d = max(max(dvr1(i,j),dvr1(i,l)),d)
        dvr2(l,j,i)=d
        r=round(trad(pts(:,i),pts(:,j),pts(:,l))+.001,3)
        k3=k3+1
        
        dch2(l,j,i) = r
        dch(k3)=r
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
write (*, '(4f6.2)') dvr2
print *
print *, "Cech"
write (*, '(4f6.2)') dch1
print *
write (*, '(4f6.2)') dch2
! print *
! write (*, '(f6.2)') dvr(1:k2)
! call hpsortn(k2,dvr)
! print *
! write (*, '(f6.2)') dvr(1:k2)
! print *
! write (*, '(f6.2)') dch(1:k3)
! call hpsortn(k3,dch)
! print *
! write (*, '(f6.2)') dch(1:k3)
call hpsort2d(k1,edges,6,6)
open(newunit=f,file='edges.txt',status='replace',action='write',iostat=f)
write (f,'(4(1x,ES19.12), 2f6.2)',err=501) edges(:,1:k1)
501 close(f)
!print *, anint(trad(pts(:,1),pts(:,4),pts(:,3))*1000.0)/1000.0
l=triangl(d1)


print *, "rank: ",l

contains
pure function trad(a,b,c)
  real, dimension(:),intent(in):: a,b,c
  
  real :: trad
  real::alpha
  trad =0.0

  alpha = acos(dot_product(b-a,c-a)/norm2(b-a)/norm2(c-a))
  trad = norm2(b-a-(c-a))/2/sin(alpha)

end function

pure function round(v,n)
  implicit none
  real, intent(in) :: v
  real:: round
  integer, intent(in) :: n
  round = anint(v*10.0**n)/10.0**n
end function round

subroutine hpsort(n,ra)
  implicit none
  integer,intent(in)::n
  real,dimension(:), intent(inout)::ra
  integer::i,ir,j,l
  real::rra

  if(n.lt.2) return
  l=n/2+1
  ir=n
10 continue
    if (l.gt.1) then
      l=l-1
      rra=ra(l)
    else
      rra=ra(ir)
      ra(ir)=ra(1)
      ir=ir-1
      if(ir.eq.1)then
        ra(1)=rra
        return
      end if
    end if
    i=l
    j=l+1
20  if (j.le.ir)then
      if(j.lt.ir)then
        if(ra(j).lt.ra(j+1))j=j+1
      end if
      if(rra.lt.ra(j))then
        ra(i)=ra(j)
        i=j
        j=j+j
      else
        
        j=ir+1
        
      end if
      goto 20
    end if
    ra(i)=rra
  goto 10
end subroutine
subroutine hpsortn(n,ra)
  implicit none
  integer,intent(inout)::n
  real,dimension(:), intent(inout)::ra
  integer::i,ir,j,l
  if (n.lt.2)return
  call hpsort(n,ra)
  l=1
  do i =2,n
    if (ra(i).gt.ra(l))then
      l=l+1
      ra(l)=ra(i)
    end if 
  end do
  n=l
end subroutine
subroutine hpsort2d(n,ra,m,c)
  implicit none
  integer,intent(in)::n,m,c
  real,dimension(:,:), intent(inout)::ra
  integer::i,ir,j,l
  real,dimension(m)::rra

  
  if(n.lt.2) return
  l=n/2+1
  ir=n
10 continue
    if (l.gt.1) then
      l=l-1
      rra=ra(:,l)
    else
      rra=ra(:,ir)
      ra(:,ir)=ra(:,1)
      ir=ir-1
      if(ir.eq.1)then
        ra(:,1)=rra
        return
      end if
    end if
    i=l
    j=l+1
20  if (j.le.ir)then
      if(j.lt.ir)then
        if(ra(c,j).lt.ra(c,j+1))j=j+1
      end if
      if(rra(c).lt.ra(c,j))then
        ra(:,i)=ra(:,j)
        i=j
        j=j+j
      else
        
        j=ir+1
        
      end if
      goto 20
    end if
    ra(:,i)=rra
  goto 10
end subroutine
end program hw2