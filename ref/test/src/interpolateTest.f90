!------------------------------------------------------------------
! interpolateTest
!
! Unit test to test interpolation software.  The origArray is
! expanded by interpFactor to interpolateArray.  The origArray is
! filled with 1.0(s), interpolation should create a larger
! array, also filled with 1.0(s) if successful.
!------------------------------------------------------------------

program interpolateTest

  use interpolate

  implicit none

  integer           :: i, j, k, ii, kk            ! do loop indexes
  integer           :: interpFactor           ! also a do loop index
  integer           :: odims(6), idims(6)     ! lower/upper bounds for 2D and 3D  arrays
  real, allocatable :: origArray2D(:, :)      ! original array, 2D
  real, allocatable :: origArray3D(:, :, :)   ! original array, 3D
  real, allocatable :: interpArray2D(:, :)    ! interpolated array
  real, allocatable :: interpArray3D(:, :, :) ! interpolated array
  real              :: epsilon
  parameter(epsilon=1e-5)

  ! 
  ! Test the 2D interpolation.
  !
  do ii = 3, 7 ! create these 2D arrays
    do interpFactor = 1, 5 ! create these interpFactor(s)

      ! Create the [ii,ii+i] 2D array.
      if (allocated(origArray2D)) then
        deallocate (origArray2D)
      endif
      allocate (origArray2D(ii, ii + 1))
      odims(1) = 1
      odims(2) = ii
      odims(3) = 1
      odims(4) = ii + 1 ! non-sqare matrix
  
      ! Populate the originalArray with 1.0
      origArray2D = 1.0

      ! Create the interpolateArray.
      call interpolateCalculateSpace2D(odims, interpFactor, idims)
      if (allocated(interpArray2D)) then
        deallocate (interpArray2D)
      endif
      allocate (interpArray2D(idims(1):idims(2), idims(3):idims(4)))
      interpArray2D = 2.0 ! populate the interpArray with 2.0 (2.0 != 1.0)
  
      ! Interpolate the interpolateArray.
      call interpolateArray2D(origArray2D, odims, interpArray2D, idims, interpFactor)
  
      ! The inperpolateArray should be all 1.0, +/- epsilon.
      ! Print success or failure.  Exit with non-zero error code if falure.
      do j = idims(3), idims(4)
        do k = idims(1), idims(2)
          if (abs(interpArray2D(k, j) - 1.0) .gt. epsilon) then
            print *, "Failure 2D"
            print *, k, j, interpArray2D(k, j), interpArray2D(k, j) - 1.0
            stop -10
          endif
        enddo
      enddo
    enddo
  enddo

  print *, "Success"

  !
  ! Test the 3D interpolation.
  !
  do kk = 2, 3
    do ii = 3, 7 ! create these 2D arrays
      do interpFactor = 1, 5 ! create these interpFactor(s)

        ! Create the [i,i+i] 2D array.
        if (allocated(origArray3D)) then
          deallocate (origArray3D)
        endif
        allocate (origArray3D(ii, ii + 1, kk))
        odims(1) = 1
        odims(2) = ii
        odims(3) = 1
        odims(4) = ii + 1 ! non-sqare matrix
        odims(5) = 1
        odims(6) = kk

        ! Populate the originalArray with 1.0
        origArray3D = 1.0

        ! Create the interpolateArray.
        call interpolateCalculateSpace3D(odims, interpFactor, idims)
        if (allocated(interpArray3D)) then
          deallocate (interpArray3D)
        endif
        allocate (interpArray3D(idims(1):idims(2), idims(3):idims(4), idims(5):idims(6)))
        interpArray3D = 2.0 ! populate the interpArray with 2.0 (2.0 != 1.0)

        ! Interpolate the interpolateArray.
        call interpolateArray3D(origArray3D, odims, interpArray3D, idims, interpFactor)
        ! The inperpolateArray should be all 1.0, +/- epsilon.
        ! Print success or failure.  Exit with non-zero error code if falure.
        do i = idims(5), idims(6)
          do j = idims(3), idims(4)
            do k = idims(1), idims(2)
              if (abs(interpArray3D(k, j, i) - 1.0) .gt. epsilon) then
                print *, "Failure 3D"
                print *, "k,j,i=", k, j, i, interpArray3D(k, j, i), interpArray3D(k, j, i) - 1.0
                stop -10
              endif
            enddo
          enddo
        enddo
      enddo
    enddo
  enddo

  print *, "Success"
  stop 0

end program interpolateTest
