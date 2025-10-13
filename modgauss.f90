module modgauss
    implicit none
    private
    public::triangl,int2str

    contains
pure function int2str(i)result(res)
    integer, intent(in)::i
     character(21)::s
    character(:), allocatable::res
    write(s, '(I20)') i
    res = trim(adjustl(s))  
end function
    function triangl(d1)
        integer::triangl
        logical,intent(inout)::d1(:,:)
        integer::i,j,l,l1
        triangl = 0
        l=1
        l1=l
        do i=1,max(size(d1(1,:)),size(d1(:,1)))
        
            do j=l+1,size(d1(1,:))
                if ((j.eq.l+1) .and. (d1(i,l))) then
                l1=l1+1
                end if
                if (d1(i,j) ) then
                    if (.not.d1(i,l)) then
                        d1(:,l:j:j-l)=d1(:,j:l:l-j)
                        l1=l1+1
                        goto 100
                    end if
                    d1(i:,j)=((d1(i:,j).or.d1(i:,l)).and.(.not.(d1(i:,j).and.d1(i:,l))))
                    !do k=m,size(t(1,:))
                    !  t(j,k)=xor(t(j,k),t(l,k))
                    !end do
                end if
    100 continue
            end do
            if (l1 .gt.l) then
                l=l1
            end if
      end do
      triangl = l-1
    end function
end module