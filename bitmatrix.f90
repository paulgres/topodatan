program bitmatrix
implicit none
integer(kind=8):: r,t

r = 45
t = ishft(r,2+56)
print *, "Hello!"
print *, t

call print_binary(t)

end program


! ==============================================================================
! Subroutine: print_binary
! Purpose: Extracts and prints the bits of an input integer value.
! ==============================================================================
subroutine print_binary(value)
    implicit none
    
    ! Input argument
    integer(kind=8), intent(in) :: value
    
    ! Local variables
    integer :: bit_count
    integer :: i
    character(len=1) :: bit_char(64) ! Array to store '0' or '1'
    character(len=256) :: binary_string
  
    ! Determine the total number of bits for this integer kind.
    ! storage_size returns the size in bits.
    bit_count = storage_size(value)
    
    ! --- Bit Extraction Loop ---
    
    ! Loop from the most significant bit (MSB) down to the least significant bit (LSB) (bit 0).
    do i = bit_count - 1, 0, -1
      ! BTEST(I, POS) returns .TRUE. if the bit at position POS of integer I is 1.
      if (btest(value, i)) then
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
    
    ! Print the final result.
    write(*, '(A)')  trim(binary_string)
    
  end subroutine print_binary