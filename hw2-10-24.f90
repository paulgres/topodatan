program hw2
  use modgauss
  use modaux
  use modmem
    implicit none

    type :: point
      integer::num
    end type
    
    real, parameter::pi=atan(1.0)*4
    integer:: n,m
    logical,allocatable,dimension(:,:)::d1,d2
    real, allocatable, dimension(:,:)::pts,edges, faces,d1vr,d2vr,d1ch,d2ch,dvr1,dch1
    real, allocatable, dimension(:)::dch, dvr,epss
    real::eps, d, r
    integer::i,j,k,k1,k2,f,l,k3
    
    !real::dvr2(n,n,n), dch2(n,n,n)
    integer::ne, lepss,b2ch, b1ch, b2vr, b1vr, b0ch, b0vr
    !pts = reshape((/ -1.,0.,0.,-2.,1.,0.,0.,2.0/), shape=(/m,n/))
    !pts = reshape((/-c,0.,0.,c,c,0.,0.,-c,3.+c,0. /), shape=(/m,n/))
    !eps = 4
open(newunit=f, file='xy.txt', status='old', action='read')
n = 0
m=2
do
  read(unit=f, fmt=*, end=11)
  n = n + 1
end do
11  continue
call allocb(d1,cnk(n,2),n)
call allocb(d2,cnk(n,3),cnk(n,2))
call alloc(dvr, cnk(n,2))
call alloc(dch, cnk(n,3)+cnk(n,2))
call alloc(epss, cnk(n,3)+2*cnk(n,2))
call allocm(dvr1,n,n)
call allocm(dch1,n,n)
call allocm(edges,m*2+2,cnk(n,2))
call allocm(faces,m*3+2,cnk(n,3))
call allocm(d1ch,cnk(n,2),n)
call allocm(d1vr,cnk(n,2),n)
call allocm(d2ch,cnk(n,3),cnk(n,2))
call allocm(d2vr,cnk(n,3),cnk(n,2))
call allocm(pts,m,n)
!goto 999
rewind(f)
read (f,'(2(1x,ES19.12))',err=504) pts
504 close(unit=f)
k1=0
k2=0
k3=0
faces=0.
edges=0.
d1ch = 0.0
d2ch = 0.0
d1vr = 0.0
d2vr = 0.0
dvr1=-1.0
dch1=-1.0 
!d1=.false.
  k=0
print *, "Start"
do i = 1,n-1

  f=0
  do j = i+1,n
    if (dvr1(j,i).le.0.0) then
      d = round(norm2(pts(:,i)-pts(:,j)),3)
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
      d1ch(k1,i) = d/2
      d1ch(k1,j) = d/2
      d1vr(k1,i) = d
      d1vr(k1,j) = d
      edges(1:m,k1) = pts(:,i)
      edges(m+1:2*m,k1) = pts(:,j)
      edges(2*m+1,k1) = d
      edges(2*m+2,k1) = d/2.0
  end do
  !goto 200
  do j = i+1, n-1
      do l = j+1,n
        d = round(norm2(pts(:,l)-pts(:,j)),3)
        !EDGES
        !j->l, i->j, i->l
        d = max(max(dvr1(j,i),dvr1(l,i)),d)
        !dvr2(l,j,i)=d
        r=round(tch(pts(:,i),pts(:,j),pts(:,l)),3)

        k=k+1
        faces(1:3*m,k) = reshape([pts(:,i),pts(:,j),pts(:,l)],[3*m])
        faces(3*m+1,k) = d
        faces(3*m+2,k) = r
        d2ch(k,nedge(i,j,n))=r
        d2ch(k,nedge(j,l,n))=r
        d2ch(k,nedge(i,l,n))=r
        d2vr(k,nedge(i,j,n))=d
        d2vr(k,nedge(j,l,n))=d
        d2vr(k,nedge(i,l,n))=d
        !dch2(l,j,i) = r
        k3=k3+1
        dch(k3)=r
      end do
    end do
  200 continue
end do


call hpsort2d(k1,edges,2*m+2,2*m+2)
call hpsort2d(k,faces,3*m+2,3*m+2)
open(newunit=f,file='edges.txt',status='replace',action='write',iostat=f)
write (f,'('// int2str(2*m)// '(1x,ES19.12), 2f6.2)',err=501) edges(:,1:k1)
501 close(f)
open(newunit=f,file='faces.txt',status='replace',action='write',iostat=f)
write (f,'('//int2str(3*m)//'(1x,ES19.12), 2f6.2)',err=502) faces(:,1:k)
502 close(f)
epss(1:k3) = dch(1:k3)
lepss = k3+k2
epss(k3+1:lepss) = dvr(1:k2)
lepss = lepss+1
epss(lepss)=.25
call hpsortn(lepss, epss)
! print *, epss(1:lepss)
! print *, dch(1:k3)
! print *, epss(1:k2)


! print *
! print *, "d1 Cech"
! write (*, '('// int2str(cnk(n,2)) //'f6.2)') d1ch
! print *
! print *, "d1 Viettoris-Rips"
! write (*, '('// int2str(cnk(n,2)) //'f6.2)') d1vr

! print *
! print *, "d2 Cech"
! write (*, '('// int2str(cnk(n,3)) //'f6.2)') d2ch
! print *
! print *, "d2 Viettoris-Rips"
! write (*, '('// int2str(cnk(n,3)) //'f6.2)') d2vr



open(newunit=f,file='betas.txt',status='replace',action='write',iostat=f)
do j = 1,lepss
  eps = epss(j)
    d1=((d1ch.gt.0.0+.001)).and.(d1ch.le.eps)
    
    d2=((d2ch.gt.0.0+.001)).and.(d2ch.le.eps)
    
    !write (*, '('// int2str(cnk(n,3)) //'l2)') d2
    k=0
    do i=1,size(d2(:,1))
      if (any(d2(i,:))) k=k+1
    end do
    l=triangl(d2)
    b2ch = k-l-0
    !print *, "rank = ", l, "; nullity = ", k-l, "; beta-2 = ", b2ch
    k2=l
    ne=0
    do i=1,size(d1(:,1))
      if (any(d1(i,:))) ne=ne+1
    end do
    !write (*, '('// int2str(cnk(n,2)) //'l2)') d1
    
    l=triangl(d1)
    b1ch = ne-l-k2
    b0ch=n-l
    !print *, "rank = ", l, "; nullity = ", ne-l, "; beta-1 = ", b1ch, "beta-0 = ", b0ch
    d1=((d1vr.gt.0.0+.001)).and.(d1vr.le.eps)
    
    d2=((d2vr.gt.0.0+.001)).and.(d2vr.le.eps)
    
    !write (*, '('// int2str(cnk(n,3)) //'l2)') d2
    k=0
    do i=1,size(d2(:,1))
      if (any(d2(i,:))) k=k+1
    end do
    l=triangl(d2)
    b2vr = k-l-0
    !print *, "rank = ", l, "; nullity = ", k-l, "; beta-2 = ", b2vr
    k2=l
    ne=0
    do i=1,size(d1(:,1))
      if (any(d1(i,:))) ne=ne+1
    end do
    !write (*, '('// int2str(cnk(n,2)) //'l2)') d1
    
    l=triangl(d1)
    b1vr = ne-l-k2
    b0vr = n-l
    !print *, "rank = ", l, "; nullity = ", ne-l, "; beta-1 = ", b1vr, "beta-0 = ", b0vr
    
    write (f,'(f6.2, 6i7)',err=503) eps, b2ch, b1ch, b2vr, b1vr, b0ch, b0vr
end do

503 close(f)

999 continue
!goto 1000
call freem(pts)
call freem(d2vr)
call freem(d2ch)
call freem(d1vr)
call freem(d1ch)
call freem(faces)
call freem(edges)
call freem(dch1)
call freem(dvr1)
call freea(dvr)
call freea(dch)
call freea(epss)
call freeb(d2)
call freeb(d1)
1000 continue
contains




end program hw2