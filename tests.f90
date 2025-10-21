program tests
    use modgauss
implicit none
integer::r
LOGICAL, DIMENSION(8, 7) :: my_matrix6,my_matrix7,my_matrix5
logical, dimension(4,4)::my_matrix4
logical, dimension(6,4)::d1,m


d1=reshape([.true., .true., .true., .false., .false., .false., &
 .true., .false., .false., .true., .true., .false., &
 .false., .true., .false., .true., .false., .true.,&
 .false., .false., .true., .false., .true., .true.],[6,4])


m=reshape([ .true., .true., .true., .true., .true., .true.,&
 .true., .true., .true., .true., .false., .true.,&
 .true., .true., .true., .true., .true., .true.,&
 .true., .true., .true., .true., .false., .true.],[6,4])
! The values in the array constructor are typically read COLUMN-WISE
! ---------------------------------------------------------------------------------------------------------------------------------
! Value List (16 elements): T, F, T, F  (Col 1),   F, T, F, T  (Col 2),   T, F, T, F  (Col 3),   F, T, F, T  (Col 4)
! ---------------------------------------------------------------------------------------------------------------------------------
! Initialize the matrix with a pattern (e.g., alternating values)
  ! We use an array constructor and RESHAPE for specific initialization
  my_matrix4 = RESHAPE( (/ .TRUE., .FALSE., .TRUE., .FALSE., &
                          .FALSE., .TRUE., .FALSE., .TRUE., &
                          .TRUE., .FALSE., .TRUE., .FALSE., &
                          .FALSE., .TRUE., .FALSE., .TRUE. /), &
                       SHAPE=(/4, 4/) )
my_matrix5 = RESHAPE( (/ .TRUE., .FALSE., .false., .true.,.false., .FALSE., .false., .true., &
                        .TRUE., .FALSE., .false., .true.,.false., .FALSE., .false., .true., &
                        .false., .TRUE., .true., .false., .false., .false., .false., .false., &
                        .false., .false., .true., .true., .false., .false., .false., .false., &
                        .false., .false., .false., .false., .true., .true., .false., .false., &
                        .false., .false., .false., .false., .false., .false., .true., .true., &
                        .false., .false., .false., .false., .false., .false., .true., .true. /), &
                     SHAPE=(/8, 7/) )

my_matrix6 = RESHAPE( (/ .TRUE., .FALSE., .false., .true.,.TRUE., .FALSE., .false., .true., &
                        .true., .TRUE., .FALSE., .false., .false., .false., .false., .false., &
                        .false., .TRUE., .true., .false., .false., .false., .false., .false., &
                        .false., .false., .true., .true., .false., .false., .false., .false., &
                        .false., .false., .false., .false., .true., .true., .false., .false., &
                        .false., .false., .false., .false., .false., .true., .true., .false., &
                        .false., .false., .false., .false., .false., .false., .true., .true. /), &
                     SHAPE=(/8, 7/) )


my_matrix7 = RESHAPE( (/ .TRUE., .FALSE., .false., .true.,.false., .FALSE., .false., .true., &
                        .true., .TRUE., .FALSE., .false., .false., .false., .false., .false., &
                        .false., .TRUE., .true., .false., .false., .false., .false., .false., &
                        .false., .false., .true., .true., .false., .false., .false., .false., &
                        .false., .false., .false., .false., .true., .true., .false., .false., &
                        .false., .false., .false., .false., .false., .true., .true., .false., &
                        .false., .false., .false., .false., .false., .false., .true., .true. /), &
                     SHAPE=(/8, 7/) )
write (*,'(7L2)') transpose(my_matrix6)
!write (*,'(4L2)') my_matrix(:,4)
print *,""
r = triangl(my_matrix6)
write (*,'(7L2)') transpose(my_matrix6)
print *, "Rank: ",r
print *,""
write (*,'(7L2)') transpose(my_matrix7)
!write (*,'(4L2)') my_matrix(:,4)
print *,""
r = triangl(my_matrix7)
write (*,'(7L2)') transpose(my_matrix7)
print *, "Rank: ",r
print *,""

write (*,'(7L2)') transpose(my_matrix5)
!write (*,'(4L2)') my_matrix(:,4)
print *,""
r = triangl(my_matrix5)
write (*,'(7L2)') transpose(my_matrix5)
print *, "Rank: ",r
print *,""

write (*,'(4L2)') transpose(my_matrix4)
!write (*,'(4L2)') my_matrix(:,4)
print *,""
r = triangl(my_matrix4)
write (*,'(4L2)') transpose(my_matrix4)
print *, "Rank: ",r
print *,""



print *,""

write (*,'(6L2)') d1
print *, "Rank: ",r
print *,""


write (*,'(6L2)') m
print *,""
write (*,'(6L2)') d1.and.m
print *,""
end program