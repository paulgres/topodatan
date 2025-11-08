program simpcomp2d
  use modgauss
  use modaux
  use modmem
    implicit none

    type :: point
      integer::num
    end type
    
    real, parameter::pi=atan(1.0)*4, delta = .001
    integer, parameter::pre=2
    integer:: n,m
    logical,allocatable,dimension(:,:)::d1,d2
    real, allocatable, dimension(:,:)::pts,edges, faces,d1vr,d2vr,dvr1
    real, allocatable, dimension(:)::dvr,epss,t(:)
    real::eps, d, start,stop
    integer::i,j,k,k1,k2,f,l,k3

    integer::ne, lepss, b2vr, b1vr, b0vr

    
f=101
open(newunit=f, file='xy_t1_1.txt', status='old', action='read')
n = 0
m=3
do
  read(unit=f, fmt=*, end=11)
  n = n + 1
end do
11  continue

call allocm(dvr1,n,n)
call allocm(edges,m*2+2,cnk(n,2))
call allocm(faces,m*3+2,cnk(n,3))
call allocm(d1vr,cnk(n,2),n)
call allocm(d2vr,cnk(n,3),cnk(n,2))
call allocm(pts,m,n)
call allocb(d1,cnk(n,2),n)
call allocb(d2,cnk(n,3),cnk(n,2))
call alloc(dvr, cnk(n,2))
call alloc(epss, cnk(n,3)+2*cnk(n,2))
call alloc(t,cnk(n,2))
!goto 999
rewind(f)
read (f,'('//int2str(m)//'(1x,ES19.12))',err=504) pts
504 close(unit=f)
k1=0
k2=0
k3=0
faces=0.
edges=0.
d1vr = 0.0
d2vr = 0.0
dvr1=-1.0
!d1=.false.
  k=0
print *, "Start evaluating epsilon for edges and faces"
call cpu_time(start)
do i = 1,n-1

  f=0
  do j = i+1,n
    if (dvr1(j,i).le.0.0) then
      d = round(norm2(pts(:,i)-pts(:,j)),pre)
      k2=k2+1
      k3=k3+1
      dvr1(j,i) = d
      dvr(k2)=d
    else
      d=dvr1(j,i)
    end if
      k1=k1+1
      d1vr(k1,i) = d
      d1vr(k1,j) = d
      edges(1:m,k1) = pts(:,i)
      edges(m+1:2*m,k1) = pts(:,j)
      edges(2*m+1,k1) = d
      edges(2*m+2,k1) = d/2.0
      
      
  end do
  do j=i+1,n-1
    do l = j+1,n
        d = round(norm2(pts(:,l)-pts(:,j)),pre)
        !EDGES
        !j->l, i->j, i->l
        d = max(max(dvr1(j,i),dvr1(l,i)),d)
        
        k=k+1
        faces(1:3*m,k) = reshape([pts(:,i),pts(:,j),pts(:,l)],[3*m])
        faces(3*m+1,k) = d
        d2vr(k,nedge(i,j,n))=d
        d2vr(k,nedge(j,l,n))=d
        d2vr(k,nedge(i,l,n))=d
        
      end do
  end do
end do
call cpu_time(stop)
print *, 'elapsed:', stop-start
call hpsort2d(k1,edges,2*m+2,2*m+1)

call hpsort2d(k,faces,3*m+2,3*m+1)
open(newunit=f,file='edges.txt',status='replace',action='write',iostat=f)
write (f,'('// int2str(2*m)// '(1x,ES19.12), 2f6.2)',err=501) edges(:,1:k1)
501 close(f)
open(newunit=f,file='faces.txt',status='replace',action='write',iostat=f)
write (f,'('//int2str(3*m)//'(1x,ES19.12), 2f6.2)',err=502) faces(:,1:k)
502 close(f)
!epss(1:k3) = dch(1:k3)
lepss = k2
epss(1:lepss) = dvr(1:k2)
lepss = lepss+1
epss(lepss) = 0.

call hpsortn(lepss, epss)
write (*, '('//int2str(cnk(n,2))//'f6.2)') transpose(d1vr)
print *
write (*, '('//int2str(cnk(n,3))//'f6.2)') transpose(d2vr)
print *

open(newunit=f,file='betas.txt',status='replace',action='write',iostat=f)
print *, "Start evaluating betas"
call cpu_time(start)
do j = 1,lepss
  eps = epss(j)
    d1=((d1vr.gt.0.0+.001)).and.(d1vr.le.eps)
    d2=((d2vr.gt.0.0+.001)).and.(d2vr.le.eps)

    k=0
    do i=1,size(d2(:,1))
      if (any(d2(i,:))) k=k+1
    end do
    print *, eps
    write (*, '('//int2str(cnk(n,3))//'l2)') transpose(d2)
    print *
    l=triangl(d2)
    b2vr = k-l-0

    k2=l
    ne=0
    do i=1,size(d1(:,1))
      if (any(d1(i,:))) ne=ne+1
    end do
    write (*, '('//int2str(cnk(n,2))//'l2)') transpose(d1)
    
    l=triangl(d1)
    b1vr = ne-l-k2
    b0vr = n-l
    
    write (f,'(f6.2, 3i7)',err=503) eps, b2vr, b1vr, b0vr
end do

call cpu_time(stop)
print *, 'elapsed:', stop-start
503 close(f)

call freea(t)
call freea(epss)
call freea(dvr)


call freeb(d2)
call freeb(d1)
call freem(pts)
call freem(d2vr)
call freem(d1vr)
call freem(faces)
call freem(edges)
call freem(dvr1)

contains



end program simpcomp2d