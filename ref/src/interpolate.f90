module interpolate
  implicit none

contains

  !*****************************************************************
  !
  ! interpolateCalculateSpace2D
  !
  ! 
  ! Subroutine for calculating the lower and upper bounds of an
  ! interpolated 2D array with regard to interpFactor.  The user is
  ! expected to call this routine, allocate the interpolated array,
  ! then call subroutine: interpolateArrray2D to fill the
  ! interpolated array.
  !
  ! Subroutine parameters:
  !   - originalArray, 2D array to be interpolated
  !   - odims, 4 elements integer array, lower/upper subscripts of original array
  !   - interpFactor, interpolation factor
  !   - odims, 4 elements integer array, lower/upper subscripts of interpolated array
  !
  ! For example:
  ! dimension interpArray(:,:)
  ! call interpolateCalculateSpace2D(odims,interpFactor,fdims)
  ! l1=fdims(1)
  ! u1=fdims(2)
  ! l2=fdims(3)
  ! l2=fdims(4)
  ! allocate (interpArray(l1:u1,l2:u2))
  ! call interpolateArray2D(originalArray, odims, interpolatedArray, fdims,
  ! interpFactor)
  !*****************************************************************

  subroutine interpolateCalculateSpace2D(odims, interpFactor, fdims)

    integer, intent(in) :: odims(4)
    integer, intent(in) :: interpFactor
    integer, intent(out):: fdims(4)

    fdims(1) = odims(1)
    fdims(3) = odims(3)
    fdims(2) = ((odims(2) - odims(1) + 1) + (odims(2) - odims(1)) * interpFactor) + (odims(1) - 1)
    fdims(4) = ((odims(4) - odims(3) + 1) + (odims(4) - odims(3)) * interpFactor) + (odims(3) - 1)

  end subroutine interpolateCalculateSpace2D

  !*****************************************************************
  !
  ! interpolateCalculateSpace3D
  !
  ! 
  ! Subroutine for calculating the lower and upper bounds of an
  ! interpolated 3D array with regard to interpFactor.  The user is
  ! expected to call this routine, allocate the interpolated array,
  ! then call subroutine: interpolateArray3D to fill the
  ! interpolated array.
  !
  !*****************************************************************

  
  subroutine interpolateCalculateSpace3D(odims, interpFactor, fdims)

    integer, intent(in) :: odims(6)
    integer, intent(in) :: interpFactor
    integer, intent(out):: fdims(6)

    fdims(1) = odims(1)
    fdims(3) = odims(3)
    fdims(5) = odims(5)
    fdims(2) = ((odims(2) - odims(1) + 1) + (odims(2) - odims(1)) * interpFactor) + (odims(1) - 1)
    fdims(4) = ((odims(4) - odims(3) + 1) + (odims(4) - odims(3)) * interpFactor) + (odims(3) - 1)
    fdims(6) = odims(6) ! The 3rd dimension is unchanged.

  end subroutine interpolateCalculateSpace3D

  !*****************************************************************
  !
  ! interpolateArray2D
  !
  ! Bilinear interpolation of f with respect to oa.
  !
  ! f's size has been calculated in subroutine interpolateCalculateSpace2D
  ! f has been allocated in the call tree above this subroutine
  !
  ! Subroutine Parameters:
  ! oa          - original array, 2D
  ! odims       - low/high subscripts for oa
  ! f           - interpolated array, 2D
  ! fdims       - low/high subscripts for f
  ! interpFactor- interpolation factor
  ! Here are some illustrations of interpFactor.
  ! We are assuming that the grid points are equally spaced.
  !
  ! An interpolation factor of 1 means 1 new interpolated element.
  ! So for example, a 3x3 matrix with interpFactor=1 becomes a 5x5 matrix
  !
  ! x's are data from oa, the o's are points to be interpolated
  !
  ! [ x x x ]     ==> [x o x o x]
  ! [ x x x ]         [o o o o o]
  ! [ x x x ]         [x o x o x]
  !                   [o o o o o]
  !                   [x o x o x]
  !
  ! And an interpFactor=2, means a 3x3 matrix becomes a 7x7 matrix.
  ! [ x x x ]     ==> [x o o x o o x]
  ! [ x x x ]         [o o o o o o o]
  ! [ x x x ]         [o o o o o o o]
  !                   [x o o x o o x]
  !                   [o o o o o o o]
  !                   [o o o o o o o]
  !                   [x o o x o o x]
  !
  !*****************************************************************

  subroutine interpolateArray2D(oa, odims, f, fdims, interpFactor)

    integer, intent(in)    :: odims(4)
    real, intent(in)       :: oa(odims(1):odims(2), odims(3):odims(4))
    integer, intent(in)    :: fdims(4)
    real, intent(inout)    :: f(fdims(1):fdims(2), fdims(3):fdims(4))
    integer, intent(in)    :: interpFactor

    ! Locals
    integer :: x1, x2, y1, y2         ! indices of the corner points
    integer :: ix, iy                 ! indices of the interpolated point
    real    :: wx1, wx2, wy1, wy2     ! weights for bilinear interpolation
    real    :: fQ11, fQ12, fQ21, fQ22 ! function values at the corners

    integer :: ol1, ou1, ol2, ou2     ! lower and upper bounds of 2D array oa
    integer :: fl1, fu1, fl2, fu2     ! lower and upper bounds of 2d array f
    integer :: i, j                   ! do loop indexes to loop over the squares

    ! Unlock the subscript bounds of the originalArray.
    ol1 = odims(1)
    ou1 = odims(2)
    ol2 = odims(3)
    ou2 = odims(4)

    ! Unlock the subscript bounds of the interpolatedArray.
    fl1 = fdims(1)
    fu1 = fdims(2)
    fl2 = fdims(3)
    fu2 = fdims(4)

    ! Intersperse the oa points into f.
    do j = ol2, ou2
      do i = ol1, ou1
        f(ol1 + (i - ol1) * (interpFactor + 1), ol2 + (j - ol2) * (interpFactor + 1)) = oa(i, j)
      end do
    end do

    ! Loop over all the squares.
    do j = fl2, fu2 - 1, interpFactor + 1
      do i = fl1, fu1 - 1, interpFactor + 1

        ! Find the indices of the corner points
        x1 = i
        x2 = i + interpFactor + 1
        y1 = j
        y2 = j + interpFactor + 1

        ! Find the value of the corner points
        fQ11 = f(x1, y1)
        fQ12 = f(x1, y2)
        fQ21 = f(x2, y1)
        fQ22 = f(x2, y2)

        ! Loop over all the interpolated points
        do iy = j, j + interpFactor + 1
          do ix = i, i + interpFactor + 1

            ! Skip the corner points
            if (((ix .eq. x1) .and. (iy .eq. y1)) .or. & 
                ((ix .eq. x1) .and. (iy .eq. y2)) .or. &
                ((ix .eq. x2) .and. (iy .eq. y1)) .or. &
                ((ix .eq. x2) .and. (iy .eq. y2))) then
              cycle
            endif

            ! Get the weights for the x direction
            wx1 = REAL(x2 - ix) / REAL(x2 - x1)
            wx2 = REAL(ix - x1) / REAL(x2 - x1)

            ! Get the weights for the y direction
            wy1 = REAL(y2 - iy) / REAL(y2 - y1)
            wy2 = REAL(iy - y1) / REAL(y2 - y1)

            ! interpolate
            f(ix, iy) = wy1 * (wx1 * fQ11 + wx2 * fQ21) + wy2 * (wx1 * fQ12 + wx2 * fQ22)

          enddo ! x loop
        enddo   ! y loop
      enddo     ! i loop
    enddo       ! j loop

  end subroutine interpolateArray2D

  !*****************************************************************
  !
  ! interpolateArray2D
  !
  ! Bilinear interpolation of f with respect to oa.  The interpolation
  ! ignores the 3rd dimension.
  !
  !*****************************************************************


  subroutine interpolateArray3D(oa, odims, f, fdims, interpFactor)

    integer, intent(in)    :: odims(6)
    real, intent(in)       :: oa(odims(1):odims(2), odims(3):odims(4), odims(5):odims(6))
    integer, intent(in)    :: fdims(6)
    real, intent(inout)    :: f(fdims(1):fdims(2), fdims(3):fdims(4), fdims(5):fdims(6))
    integer, intent(in)    :: interpFactor

    ! Locals.
    integer :: i

    do i=odims(5), odims(6)
       call interpolateArray2D(oa(:,:,i), odims, f(:,:,i), fdims, interpFactor) 
    enddo

  end subroutine interpolateArray3D

end module interpolate
