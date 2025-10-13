program tests
    use modgauss
implicit none
integer::r
LOGICAL, DIMENSION(8, 7) :: my_matrix

! The values in the array constructor are typically read COLUMN-WISE
! ---------------------------------------------------------------------------------------------------------------------------------
! Value List (16 elements): T, F, T, F  (Col 1),   F, T, F, T  (Col 2),   T, F, T, F  (Col 3),   F, T, F, T  (Col 4)
! ---------------------------------------------------------------------------------------------------------------------------------
my_matrix = RESHAPE( (/ .TRUE., .FALSE., .false., .true.,.TRUE., .FALSE., .false., .true., &
                        .true., .TRUE., .FALSE., .false., .false., .false., .false., .false., &
                        .false., .TRUE., .true., .false., .false., .false., .false., .false., &
                        .false., .false., .true., .true., .false., .false., .false., .false., &
                        .false., .false., .false., .false., .true., .true., .false., .false., &
                        .false., .false., .false., .false., .false., .true., .true., .false., &
                        .false., .false., .false., .false., .false., .false., .true., .true. /), &
                     SHAPE=(/8, 7/) )

write (*,'(7L2)') transpose(my_matrix)
!write (*,'(4L2)') my_matrix(:,4)
print *,""
r = triangl(my_matrix)
write (*,'(7L2)') transpose(my_matrix)
print *, "Rank: ",r
print *,""
end program