program bitmatrix
  use modmem
implicit none
integer(kind=8)::msize = 16384
integer(kind=8),allocatable::t(:,:)
integer(kind=8):: r,r2(4),r3(4),i,j,k,m,n,p,l,l1
real(kind=8),allocatable::t1(:,:)
real::start,stop
call alloci(t,msize*64,msize)
call allocm(t1,msize*64,msize)

! r2(1) = 3
! r3(1)= 3
! r2(1) = ishft(r2(1), 6+56)
! r3(1) = ishft(r3(1), 5+56)
! t(1,1) = ishft(r,2+56)
! t(2,1) = 50

!call print_binary(t(1,1))
!call axor(t(1,:),r2)
!call axor(t(1,:),r3)
call random_number(t1)
!write (*,'(4f8.4)') t1
t=floor(t1*8446744073709551615.0,8)
!write (*,'(4I21)') t
!t(1:2:2-1,:)=t(2:1:1-2,:)
!call print_binary(t(1,1))
!call print_binary(t(2,1))

write (*,'(3I5)') size(t(1,:)), size(t(:,1)), min(8*8*size(t(1,:)),size(t(:,1)))
l=1
l1=l
! do j=1,size(t(:,1))
!   call print_binary(t(j,:))
! end do
print *,""

call cpu_time(start)

do i=1,min(8*8*size(t(1,:)),size(t(:,1)))-1
  n = mod(i-1,8*8)+1
  m = (i-1)/(8*8)+1
  if (n .eq. 1) then
    p = 1
    p = ishft(p,63)
  else
    p = ishft(p,-1)
  end if
  do j=l+1,size(t(:,1))
    if ((j.eq.l+1) .and. (and(p,t(l,m)) .ne. 0)) then
      l1=l1+1
    end if
    if (and(p,t(j,m)) .ne. 0) then
      if (and(p,t(l,m)) .eq. 0) then
        t(l:j:j-l,:)=t(j:l:l-j,:)
        l1=l1+1
        goto 100
      end if
      do k=m,size(t(1,:))
        t(j,k)=xor(t(j,k),t(l,k))
      end do
    end if
100 continue
  end do
  if (l1 .gt.l) then
    
    ! if (i.eq.63) then
    !   do j=1,size(t(:,1))
    !     call print_binary(t(j,:))
    !   end do
    !   goto 200
    ! end if
    l=l1
  end if
end do
call cpu_time(stop)
    print *, 'elapsed:', stop-start
    call freem(t1)
    call freei(t)
write (*,'(A,I5)') "Rank: ",l-1
! do j=1,size(t(:,1))
!         call print_binary(t(j,:))
! end do
200 continue

contains
 subroutine axor(r,b)
    implicit none
    integer(kind=8), intent(inout):: r(:)
    integer(kind=8), intent(in)::b(:)
    integer(kind=8)::i,n
    n = size(r)
    do i=1,n
      r(i)=xor(r(i),b(i))
    end do
  end subroutine
  ! ==============================================================================
  ! Subroutine: print_binary
  ! Purpose: Extracts and prints the bits of an input integer value.
  ! ==============================================================================
  subroutine print_binary(value)
    implicit none
    
    ! Input argument
    integer(kind=8),dimension(:), intent(in) :: value
    
    ! Local variables
    integer :: bit_count
    integer :: i,j,n
    character(len=1) :: bit_char(64) ! Array to store '0' or '1'
    character(len=256) :: binary_string
    character(len=64) :: res(4)
  
    n = size(value)
    do j=1,n
    ! Determine the total number of bits for this integer kind.
    ! storage_size returns the size in bits.
    bit_count = storage_size(value(j))
    
    ! --- Bit Extraction Loop ---
    
    ! Loop from the most significant bit (MSB) down to the least significant bit (LSB) (bit 0).
    do i = bit_count - 1, 0, -1
      ! BTEST(I, POS) returns .TRUE. if the bit at position POS of integer I is 1.
      if (btest(value(j), i)) then
        bit_char(bit_count - i) = '1'
      else
        bit_char(bit_count - i) = '0'
      end if
    end do
  
    ! Convert the character array back into a single string for cleaner output.
    binary_string = ""
    do i = 1, bit_count
      binary_string = trim(binary_string) // bit_char(i)
    end do
    
    ! Optional: Add spaces for readability (e.g., separating bytes)
    do i = (bit_count / 8) * 8, 8, -8
       if (i < bit_count) then
           binary_string = binary_string(1:i) // "" // binary_string(i+1:)
       end if
    end do
    res(j) = trim(binary_string)
  end do 
    ! Print the final result.
    write(*, '(A)')  res(1)
    
  end subroutine print_binary
end program

 
