program hw2
  use modgauss
  use modaux
    implicit none

    type :: point
      integer::num
    end type
    
    integer, parameter:: n=4,m=2
    logical::d1(n*(n-1)/2,n),fa(n,n,n),ed(n,n)
    logical::d2(n*(n-1)*(n-2)/6,n*(n-1)/2)
    integer::i,j,k,tt(n),k1,k2,f,l,k3
    real:: eps,d,r,edges(m*2+2,n*(n-1)/2),d1r(n*(n-1)/2,n),dvr1(n,n),dch1(n,n) 
    real::dvr2(n,n,n), dch2(n,n,n), dch(n*(n-1)),dvr(n*(n-1)/2)
    real::faces(m*3+2,n*(n-1)*(n-2)/6),d2r(n*(n-1)*(n-2)/6,n*(n-1)/2)
    real, dimension(m,n) ::pts
    pts = reshape((/ -1.,0.,0.,-2.,1.,0.,0.,2./), shape=(/m,n/))


    eps = 4
    write (*, '(2f6.2)') pts
k1=0
k2=0
k3=0
d1r = 0.0
d2r = 0.0
dvr1=-1.0
dch1=-1.0 
dvr2=-1.0
dch2=-1.0 
d1=.false.
  k=0
do i = 1,n-1

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
      d1r(k1,i) = d/2
      d1r(k1,j) = d/2
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
        !EDGES
        !j->l, i->j, i->l
        d = max(max(dvr1(j,i),dvr1(l,i)),d)
        dvr2(l,j,i)=d
        r=round(trad(pts(:,i),pts(:,j),pts(:,l))+.001,3)

        k=k+1
        faces(1:6,k) = reshape([pts(:,i),pts(:,j),pts(:,l)],[6])
        faces(7,k) = d
        faces(8,k) = r
        d2r(k,nedge(i,j,n))=r
        d2r(k,nedge(j,l,n))=r
        d2r(k,nedge(i,l,n))=r
        dch2(l,j,i) = r
        k3=k3+1
        dch(k3)=r
      end do
    end do
  
end do
d1=((d1r.gt.0.0+.001).and.(d1r.le.1.25+.001))

d2=((d2r.gt.0.0)).and.(d2r.le.1.25+.001)
print *, "k1 =  ", k1
print *, "k2 =  ", k2
print *, "k3 =  ", k3
print *
print *, "Viettoris-Rips"
write (*, '('//int2str(n)//'f6.2)') dvr1
print *
write (*, '('//int2str(n)//'f6.2)') dvr2
print *
print *, "Cech"
write (*, '('//int2str(n)//'f6.2)') dch1
print *
write (*, '('//int2str(n)//'f6.2)') dch2
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
call hpsort2d(k,faces,8,8)
open(newunit=f,file='edges.txt',status='replace',action='write',iostat=f)
write (f,'(4(1x,ES19.12), 2f6.2)',err=501) edges(:,1:k1)
501 close(f)
open(newunit=f,file='faces.txt',status='replace',action='write',iostat=f)
write (f,'(6(1x,ES19.12), 2f6.2)',err=502) faces(:,1:k)
502 close(f)
!print *, anint(trad(pts(:,1),pts(:,4),pts(:,3))*1000.0)/1000.0



print *
write (*, '('// int2str(cnk(n,2)) //'f6.2)') d1r
print *
write (*, '('// int2str(cnk(n,2)) //'l2)') ((d1r.gt.0.0+.001))!.and.(d1r.le.1.15+.001)
print *
write (*, '('// int2str(cnk(n,2)) //'l2)') (d1r.le.1.15+.001)
print *
write (*, '('// int2str(cnk(n,2)) //'l2)') d1
l=triangl(d1)


print *, "rank: ",l
print *
write (*, '('// int2str(cnk(n,3)) //'f6.2)') d2r
print *
write (*, '('// int2str(cnk(n,3)) //'l2)') d2 
print *, triangl(d2)

contains




end program hw2