module modaux
    implicit none
    private
    public::int2str,round,trad,hpsort,hpsort2d,hpsortn,cnk,nedge,tch
    real::round,trad,tch
    integer::cnk,nedge
    
contains
pure function nedge(i,j,n)

integer,intent(in)::i,j,n
nedge = n*(i-1)+j-i*(i+1)/2
end function
  pure function cnk(n,k)
  integer, intent(in)::n,k
  integer::i,j
  cnk=n
  j=k
  do i=1,k-1
    cnk=cnk*(n-i)
    j=j*i
  end do
  cnk=cnk/j
end function
  pure function int2str(i)result(res)
    integer, intent(in)::i
     character(21)::s
    character(:), allocatable::res
    write(s, '(I20)') i
    res = trim(adjustl(s))  
  end function
  recursive function factorial(n)result(r)
integer::r
integer,intent(in)::n
if (n.eq.2)then
  r=2
else
  r = n*factorial(n-1)
end if
end function factorial


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

pure function cosv(v,u)
  real, dimension(:),intent(in)::v,u
  real::cosv
  cosv = dot_product(v,u)/norm2(v)/norm2(u)
end function

function cos3(a,b,c)
  real,dimension(:),intent(in):: a,b,c
  real,dimension(2)::u,v
  real::cos3
  write (*,'(6f8.4)') a,b,c
  cos3 = dot_product(b-a,c-a)/norm2(b-a)/norm2(c-a)!cosv(u,v)
end function

function tch(a,b,c)
  real,dimension(:),intent(in):: a,b,c
  real,dimension(2)::m
  if (cos3(a,b,c).le.0.0) then 
    tch = norm2(b-c)/2.
  else if (cos3(b,a,c).le.0.0) then 
    tch = norm2(a-c)/2.
  else if (cos3(c,a,b).le.0.0) then 
    tch = norm2(a-b)/2.
  else
    tch = trad(a,b,c)
  end if 
end function

! pure function tmass(a,b,c)

!   real,dimension(:),intent(in):: a,b,c
!   real,dimension(2)::m
!   real::n1,n3,n2
!   m=(b-a+c-a)/3+a
!   n1=norm2((b-a+c-a)/3)
!   n2=norm2((a-b+c-b)/3)
!   n3=norm2((b-c+a-c)/3)
!   tmass = max(n1,n2,n3)
! end function 

pure function trad(a,b,c)
  real, dimension(:),intent(in):: a,b,c
  
  
  real::alpha
  trad =0.0

  alpha = acos(dot_product(b-a,c-a)/norm2(b-a)/norm2(c-a))
  trad = norm2(b-a-(c-a))/2/sin(alpha)

end function

pure function round(v,n)
  implicit none
  real, intent(in) :: v
  
  integer, intent(in) :: n
  round = anint(v*10.0**n)/10.0**n
end function round
end module modaux