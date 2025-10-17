module inbox_funcs
  implicit none
contains
  !*******************************************************************************
  logical FUNCTION inbox( x, y, pts )
    ! A subroutine to solve my old "is the point inside the box? conundrum.
    ! Find the arrangement of the points, the calculate there slope of each side.
    ! Then check that the point is on the correct side of each line (line == side).
    !---------NOTE: this program assumes that points on the line are *IN* the box.
    !*******************************************************************************

    real, INTENT(IN) :: x, y
    real, dimension(1:4,1:2), INTENT(IN) :: pts
    ! FUNCTIONS---------------------------------------
    ! LOCAL VARIABLES---------------------------------
    real :: m1, m2, m3, m4, val1, val2, val3, val4
    real, dimension(1:2) :: p1, p2, p3, p4

    p1(1) = pts(1,1)
    p1(2) = pts(1,2)
    p2(1) = pts(2,1)
    p2(2) = pts(2,2)
    p3(1) = pts(3,1)
    p3(2) = pts(3,2)
    p4(1) = pts(4,1)
    p4(2) = pts(4,2)

!!!! Sides can be built sequentially this way, 1 is adjacent to 2, which
!!!! is adjacent to 3, which is to 4, which is to 1 again. So slope 1 and
!!!! 3 and 2 and 4 will always be the opposite sides. This makes point-in
!!!! -box decision easier later.
    IF ( ((p2(2)-p1(2)) / (p2(1)-p1(1))) /= ((p4(2)-p3(2)) / (p4(1)-p3(1))) ) THEN
       !   then segment 12 should be a diagonal of the box,
       !   and thus segment 34 will be the other diagonal
       m1= (p3(2)-p1(2)) / (p3(1)-p1(1))
       m2 = (p2(2)-p3(2)) / (p2(1)-p3(1))
       m3 = (p4(2)-p2(2)) / (p4(1)-p2(1))
       m4 = (p1(2)-p4(2)) / (p1(1)-p4(1))
       val1 = y - m1 * (x - p1(1)) + p1(2)
       val2 = y - m2 * (x - p2(1)) + p2(2)
       val3 = y - m3 * (x - p2(1)) + p2(2)
       val4 = y - m4 * (x - p1(1)) + p1(2)
       ! Now that we have the above/below-line values, they should be of
       ! opposite signs for opposite sides
       IF ( ((val1 > 0).AND.(val3 < 0) .OR. (val1 < 0).AND.(val3 > 0)) .AND. &
            ((val2 > 0).AND.(val4 < 0) .OR. (val2 < 0).AND.(val4 > 0)) ) THEN
          inbox = .TRUE.
       ELSE
          inbox = .FALSE.
       ENDIF
    ELSEIF ( ((p3(2)-p1(2)) / (p3(1)-p1(1))) /= ((p4(2)-p2(2)) / (p4(1)-p2(1))) ) THEN
       !   then segment 13 should be a diagonal of the box,
       !   ...
       m1 = (p2(2)-p1(2)) / (p2(1)-p1(1))
       m2 = (p3(2)-p2(2)) / (p3(1)-p2(1))
       m3 = (p4(2)-p3(2)) / (p4(1)-p3(1))
       m4 = (p1(2)-p4(2)) / (p1(1)-p4(1))
       val1 = y - m1 * (x - p1(1)) + p1(2)
       val2 = y - m2 * (x - p3(1)) + p3(2)
       val3 = y - m3 * (x - p3(1)) + p3(2)
       val4 = y - m4 * (x - p1(1)) + p1(2)
       IF ( ((val1 >= 0).AND.(val3 <= 0) .OR. (val1 <= 0).AND.(val3 >= 0)) .AND. &
            ((val2 >= 0).AND.(val4 <= 0) .OR. (val2 <= 0).AND.(val4 >= 0)) ) THEN
          inbox = .TRUE.
       ELSE
          inbox = .FALSE.
       ENDIF
    ELSEIF ( ((p4(2)-p1(2)) / (p4(1)-p1(1))) /= ((p3(2)-p2(2)) / (p3(1)-p2(1))) ) THEN
       !   then segment 14 should be a diagonal of the box,
       !   ...
       m1 = (p2(2)-p1(2)) / (p2(1)-p1(1))
       m2 = (p4(2)-p2(2)) / (p4(1)-p2(1))
       m3 = (p3(2)-p4(2)) / (p3(1)-p4(1))
       m4 = (p1(2)-p3(2)) / (p1(1)-p3(1))
       val1 = y - m1 * (x - p1(1)) + p1(2)
       val2 = y - m2 * (x - p4(1)) + p4(2)
       val3 = y - m3 * (x - p4(1)) + p4(2)
       val4 = y - m4 * (x - p1(1)) + p1(2)
       IF ( ((val1 >= 0).AND.(val3 <= 0) .OR. (val1 <= 0).AND.(val3 >= 0)) .AND. &
            ((val2 >= 0).AND.(val4 <= 0) .OR. (val2 <= 0).AND.(val4 >= 0)) ) THEN
          inbox = .TRUE.
       ELSE
          inbox = .FALSE.
       ENDIF
    ENDIF

  END FUNCTION inbox
  !===============================================================================

  !*******************************************************************************
!!$  logical FUNCTION inbox2( x, y, pts )
!!$    ! Same as above, put a different implementation idea I found on the internet.
!!$    ! Extend a ray along the positive X-axis, and see if it intersects with any of
!!$    ! vertices of the box.
!!$    ! See http://tog.acm.org/editors/erich/ptinpoly/
!!$    !*******************************************************************************
!!$    real, INTENT(IN) :: x, y
!!$    real, dimension(1:2,1:4), INTENT(IN) :: pts
!!$    inbox2 = .TRUE. ! it's a work in progress...
!!$  END FUNCTION inbox2
  !===============================================================================

  !*******************************************************************************

  !===============================================================================

  !*******************************************************************************

  !===============================================================================
end module inbox_funcs
