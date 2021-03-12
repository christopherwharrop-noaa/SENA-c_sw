!***********************************************************************
!*                   GNU Lesser General Public License
!*
!* This file is part of the FV3 dynamical core.
!*
!* The FV3 dynamical core is free software: you can redistribute it
!* and/or modify it under the terms of the
!* GNU Lesser General Public License as published by the
!* Free Software Foundation, either version 3 of the License, or
!* (at your option) any later version.
!*
!* The FV3 dynamical core is distributed in the hope that it will be
!* useful, but WITHOUT ANYWARRANTY; without even the implied warranty
!* of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
!* See the GNU General Public License for more details.
!*
!* You should have received a copy of the GNU Lesser General Public
!* License along with the FV3 dynamical core.
!* If not, see <http://www.gnu.org/licenses/>.
!***********************************************************************

module sw_core_mod
 
  use netCDFModule
#ifdef ENABLE_GPTL
  use gptl
#endif

  implicit none

  integer :: do_profile = 0  ! Flag for enabling profiling at runtime

  real, parameter:: big_number = 1.E8

  ! 4-pt Lagrange interpolation
  real, parameter:: a1 =  0.5625
  real, parameter:: a2 = -0.0625

  ! volume-conserving cubic with 2nd drv=0 at end point:
  real, parameter:: c1 = -2./14.
  real, parameter:: c2 = 11./14.
  real, parameter:: c3 =  5./14.

  ! Array domain indices
  integer :: isd
  integer :: ied
  integer :: jsd
  integer :: jed
  integer :: is
  integer :: ie
  integer :: js
  integer :: je
  integer :: nord
  integer :: npx
  integer :: npy
  integer :: npz

  ! State variables
  logical           :: sw_corner, se_corner, ne_corner, nw_corner
  real              :: dt2
  real, allocatable :: rarea(:,:), rarea_c(:,:) 
  real, allocatable :: sin_sg(:,:,:), cos_sg(:,:,:)
  real, allocatable :: sina_v(:,:), cosa_v(:,:)
  real, allocatable :: sina_u(:,:), cosa_u(:,:)
  real, allocatable :: fC(:,:)
  real, allocatable :: rdxc(:,:), rdyc(:,:)
  real, allocatable :: dx(:,:), dy(:,:)
  real, allocatable :: dxc(:,:), dyc(:,:)
  real, allocatable :: cosa_s(:,:)
  real, allocatable :: rsin_u(:,:), rsin_v(:,:)
  real, allocatable :: rsin2(:,:)
  real, allocatable :: dxa(:,:), dya(:,:)
  real, allocatable :: delpc(:,:,:), delp(:,:,:)
  real, allocatable :: ptc(:,:,:), pt(:,:,:)
  real, allocatable :: u(:,:,:), v(:,:,:), w(:,:,:)
  real, allocatable :: uc(:,:,:), vc(:,:,:), wc(:,:,:)
  real, allocatable :: ua(:,:,:), va(:,:,:)
  real, allocatable :: ut(:,:,:), vt(:,:,:)
  real, allocatable :: divg_d(:,:,:)

contains

  !------------------------------------------------------------------
  ! c_sw
  !
  ! Top-level routine for c_sw kernel
  !------------------------------------------------------------------
  subroutine c_sw(sw_corner, se_corner, nw_corner, ne_corner,       &
                  rarea, rarea_c, sin_sg, cos_sg, sina_v, cosa_v,   &
                  sina_u, cosa_u, fC, rdxc, rdyc, dx, dy, dxc, dyc, &
                  cosa_s, rsin_u, rsin_v, rsin2, dxa, dya, delpc,   &
                  delp, ptc, pt, u, v, w, uc, vc, ua, va, wc, ut,   &
                  vt, divg_d, dt2)

    logical, intent(   in) :: sw_corner, se_corner, ne_corner, nw_corner
    real,    intent(   in), dimension(isd:ied,  jsd:jed    ) :: rarea
    real,    intent(   in), dimension(isd:ied+1,jsd:jed+1  ) :: rarea_c
    real,    intent(   in), dimension(isd:ied,  jsd:jed,  9) :: sin_sg, cos_sg
    real,    intent(   in), dimension(isd:ied,  jsd:jed+1  ) :: sina_v, cosa_v
    real,    intent(   in), dimension(isd:ied+1,jsd:jed    ) :: sina_u, cosa_u
    real,    intent(   in), dimension(isd:ied+1,jsd:jed+1  ) :: fC
    real,    intent(   in), dimension(isd:ied+1,jsd:jed    ) :: rdxc, dy, dxc
    real,    intent(   in), dimension(isd:ied,  jsd:jed+1  ) :: rdyc, dx, dyc
    real,    intent(   in), dimension(isd:ied,  jsd:jed    ) :: cosa_s
    real,    intent(   in), dimension(isd:ied+1,jsd:jed    ) :: rsin_u
    real,    intent(   in), dimension(isd:ied,  jsd:jed+1  ) :: rsin_v
    real,    intent(   in), dimension(isd:ied,  jsd:jed    ) :: rsin2, dxa, dya
    real,    intent(inout), dimension(isd:ied,  jsd:jed+1  ) :: u, vc
    real,    intent(inout), dimension(isd:ied+1,jsd:jed    ) :: v, uc
    real,    intent(inout), dimension(isd:ied,  jsd:jed    ) :: delp, pt, ua
    real,    intent(inout), dimension(isd:ied,  jsd:jed    ) :: va, ut, vt, w
    real,    intent(  out), dimension(isd:ied,  jsd:jed    ) :: delpc, ptc, wc
    real,    intent(  out), dimension(isd:ied+1,jsd:jed+ 1 ) :: divg_d
    real,    intent(   in)                                   :: dt2

    ! Local:
    real, dimension(is-1:ie+1, js-1:je+1) :: vort, ke
    real, dimension(is-1:ie+2, js-1:je+1) :: fx, fx1, fx2
    real, dimension(is-1:ie+1, js-1:je+2) :: fy, fy1, fy2
    real                                  :: dt4
    integer                               :: i, j
    integer                               :: iep1, jep1
    integer                               :: ret

#ifdef ENABLE_GPTL
  if (do_profile == 1) then
    ret = gptlstart('c_sw')
  end if
#endif

    iep1 = ie + 1
    jep1 = je + 1

    call d2a2c_vect(sw_corner, se_corner, ne_corner, nw_corner,            &
                    sin_sg, cosa_u, cosa_v, cosa_s, rsin_u, rsin_v, rsin2, &
                    dxa, dya, u, v, ua, va, uc, vc, ut, vt)

    if ( nord > 0 ) then
      call divergence_corner(sw_corner, se_corner, ne_corner, nw_corner, &
                             rarea_c, sin_sg, cos_sg,                    &
                             dxc, dyc, u, v, ua, va, divg_d)
    endif

    do j = js-1, jep1
      do i = is-1, iep1+1
        if (ut(i, j) > 0.) then
              ut(i, j) = dt2 * ut(i, j) * dy(i, j) * sin_sg(i-1, j, 3)
        else
              ut(i, j) = dt2 * ut(i, j) * dy(i, j) * sin_sg(i, j, 1)
        end if
      enddo
    enddo
    do j = js-1, je+2
      do i = is-1, iep1
        if (vt(i, j) > 0.) then
          vt(i, j) = dt2 * vt(i, j) * dx(i, j) * sin_sg(i, j-1, 4)
        else
          vt(i, j) = dt2 * vt(i, j) * dx(i, j) * sin_sg(i, j, 2)
          end if
      enddo
    enddo

    !----------------
    ! Transport delp:
    !----------------
    ! Xdir:
    call fill2_4corners(delp, pt, 1, sw_corner, se_corner, ne_corner, nw_corner)
    call fill_4corners(w, 1, sw_corner, se_corner, ne_corner, nw_corner)
    do j = js-1, je+1
      do i = is-1, ie+2
        if ( ut(i, j) > 0. ) then
          fx1(i, j) = delp(i-1, j)
          fx(i, j) = pt(i-1, j)
          fx2(i, j) = w(i-1, j)
        else
          fx1(i, j) = delp(i, j)
          fx(i, j) = pt(i, j)
          fx2(i, j) = w(i, j)
        endif
        fx1(i, j) = ut(i, j) * fx1(i, j)
        fx(i, j) = fx1(i, j) * fx(i, j)
        fx2(i, j) = fx1(i, j) * fx2(i, j)
      enddo
    enddo

    ! Ydir:
    call fill2_4corners(delp, pt, 2, sw_corner, se_corner, ne_corner, nw_corner)
    call fill_4corners(w, 2, sw_corner, se_corner, ne_corner, nw_corner)
    do j = js-1, je+2
       do i = is-1, ie+1
        if ( vt(i, j) > 0. ) then
          fy1(i, j) = delp(i, j-1)
          fy(i, j) = pt(i, j-1)
          fy2(i, j) = w(i, j-1)
        else
          fy1(i, j) = delp(i, j)
          fy(i, j) = pt(i, j)
          fy2(i, j) = w(i, j)
        endif
        fy1(i, j) =  vt(i, j) * fy1(i, j)
        fy(i, j) = fy1(i, j) * fy(i, j)
        fy2(i, j) = fy1(i, j) * fy2(i, j)
      enddo
    enddo
    do j = js-1, je+1
      do i = is-1, ie+1
        delpc(i, j) = delp(i, j) + (fx1(i, j) - fx1(i+1, j) + fy1(i, j) - &
                      fy1(i, j+1)) * rarea(i, j)
        ptc(i, j) = (pt(i, j) * delp(i, j) + (fx(i, j) - fx(i+1, j) +     &
                     fy(i, j) - fy(i, j+1)) * rarea(i, j)) / delpc(i,j)
        wc(i, j) = (w(i, j) * delp(i, j) + (fx2(i, j) - fx2(i+1, j) +     &
                    fy2(i, j) - fy2(i, j+1)) * rarea(i, j)) / delpc(i, j)
      enddo
    enddo

    !------------
    ! Compute KE:
    !------------

    ! Since uc = u*, i.e. the covariant wind perpendicular to the face edge, if
    ! we want to compute kinetic energy we will need the true coordinate-parallel
    ! covariant wind, computed through u = uc*sina + v*cosa.
    !
    ! Use the alpha for the cell KE is being computed in.
    do j = js-1, jep1
      do i = is-1, iep1
        if ( ua(i, j) > 0. ) then
          if ( i == 1 ) then
            ke(1, j) = uc(1, j) * sin_sg(1, j, 1) + v(1, j) * cos_sg(1, j, 1)
          elseif ( i == npx  ) then
            ke(i, j) = uc(npx, j) * sin_sg(npx, j, 1) + v(npx, j) * cos_sg(npx, j, 1)
          else
            ke(i, j) = uc(i, j)
          endif
        else
          if ( i == 0 ) then
            ke(0, j) = uc(1, j) * sin_sg(0, j, 3) + v(1, j) * cos_sg(0, j, 3)
          elseif ( i == (npx - 1) ) then
            ke(i, j) = uc(npx, j) * sin_sg(npx-1, j, 3) + v(npx, j) * cos_sg(npx-1, j, 3)
          else
            ke(i, j) = uc(i+1, j)
          endif
        endif
      enddo
    enddo
    do j = js-1, jep1
      do i = is-1, iep1
        if ( va(i, j) > 0. ) then
          if ( j == 1 ) then
            vort(i, 1) = vc(i, 1) * sin_sg(i, 1, 2) + u(i, 1) * cos_sg(i, 1, 2)
          elseif ( j == npy ) then
            vort(i, j) = vc(i, npy) * sin_sg(i, npy, 2) + u(i, npy) * cos_sg(i, npy, 2)
          else
            vort(i, j) = vc(i, j)
          endif
        else
          if ( j == 0 ) then
            vort(i, 0) = vc(i, 1) * sin_sg(i, 0, 4) + u(i, 1) * cos_sg(i, 0, 4)
          elseif ( j == (npy - 1) ) then
            vort(i, j) = vc(i, npy) * sin_sg(i, npy-1, 4) + u(i, npy) * cos_sg(i, npy-1, 4)
          else
            vort(i, j) = vc(i, j+1)
          endif
        endif
      enddo
    enddo

    dt4 = 0.5 * dt2
    do j = js-1, jep1
      do i = is-1, iep1
        ke(i, j) = dt4 * (ua(i, j) * ke(i, j) + va(i, j) * vort(i, j))
      enddo
    enddo

    !------------------------------
    ! Compute circulation on C grid
    !------------------------------
    ! To consider using true co-variant winds at face edges?
    do j = js-1, je+1
      do i = is, ie+1
        fx(i, j) = uc(i, j) * dxc(i, j)
      enddo
    enddo

    do j = js, je+1
      do i = is-1, ie+1
        fy(i, j) = vc(i, j) * dyc(i, j)
      enddo
    enddo

    do j = js, je+1
      do i = is, ie+1
        vort(i, j) =  fx(i, j-1) - fx(i, j) - fy(i-1, j) + fy(i, j)
      enddo
    enddo

    ! Remove the extra term at the corners:
    if ( sw_corner ) vort(1,     1) = vort(1,     1) + fy(0,     1)
    if ( se_corner ) vort(npx,   1) = vort(npx,   1) - fy(npx,   1)
    if ( ne_corner ) vort(npx, npy) = vort(npx, npy) - fy(npx, npy)
    if ( nw_corner ) vort(1,   npy) = vort(1,   npy) + fy(0,   npy)

    !----------------------------
    ! Compute absolute vorticity
    !----------------------------
    do j = js, je+1
      do i = is, ie+1
        vort(i, j) = fC(i, j) + rarea_c(i, j) * vort(i, j)
      enddo
    enddo

    !----------------------------------
    ! Transport absolute vorticity:
    !----------------------------------
    ! To go from v to contravariant v at the edges, we divide by sin_sg;
    ! but we then must multiply by sin_sg to get the proper flux.
    ! These cancel, leaving us with fy1 = dt2*v at the edges.
    ! (For the same reason we only divide by sin instead of sin**2 in the interior)
    do j = js, je
!DEC$ VECTOR ALWAYS
      do i = is, iep1
        if ( i == 1 .or. i == npx ) then
          fy1(i, j) = dt2 * v(i, j)
        else
          fy1(i, j) = dt2 * (v(i, j) - uc(i, j) * cosa_u(i, j)) / sina_u(i, j)
        endif
        if ( fy1(i, j) > 0. ) then
          fy(i, j) = vort(i, j)
        else
          fy(i, j) = vort(i, j+1)
        endif
      enddo
    enddo
    do j = js, jep1
      if ( j == 1 .or. j == npy ) then
!DEC$ VECTOR ALWAYS
        do i = is, ie
          fx1(i, j) = dt2 * u(i, j)
          if ( fx1(i, j) > 0. ) then
            fx(i, j) = vort(i, j)
          else
            fx(i, j) = vort(i+1, j)
          endif
        enddo
      else
!DEC$ VECTOR ALWAYS
        do i = is, ie
          fx1(i, j) = dt2 * (u(i, j) - vc(i, j) * cosa_v(i, j)) / sina_v(i, j)
          if ( fx1(i, j) > 0. ) then
            fx(i, j) = vort(i, j)
          else
            fx(i, j) = vort(i+1, j)
          endif
        enddo
      endif
    enddo

    ! Update time-centered winds on the C-Grid
    do j = js, je
      do i = is, iep1
        uc(i, j) = uc(i, j) + fy1(i, j) * fy(i, j) + rdxc(i, j) * (ke(i-1, j) - ke(i, j))
      enddo
    enddo
    do j = js, jep1
      do i = is, ie
        vc(i, j) = vc(i, j) - fx1(i, j) * fx(i, j) + rdyc(i, j) * (ke(i, j-1) - ke(i, j))
      enddo
    enddo

#ifdef ENABLE_GPTL
  if (do_profile == 1) then
    ret = gptlstop('c_sw')
  end if
#endif

  end subroutine c_sw


  !------------------------------------------------------------------
  ! divergence_corner
  !------------------------------------------------------------------
  subroutine divergence_corner(sw_corner, se_corner, ne_corner, nw_corner, &
                               rarea_c, sin_sg, cos_sg,                    &
                               dxc, dyc, u, v, ua, va, divg_d)

    logical, intent( in) :: sw_corner, se_corner, ne_corner, nw_corner
    real,    intent( in), dimension(isd:ied+1, jsd:jed+1  ) :: rarea_c
    real,    intent( in), dimension(isd:ied,   jsd:jed,  9) :: sin_sg, cos_sg
    real,    intent( in), dimension(isd:ied+1, jsd:jed    ) :: dxc
    real,    intent( in), dimension(isd:ied,   jsd:jed+1  ) :: dyc
    real,    intent( in), dimension(isd:ied,   jsd:jed+1  ) :: u
    real,    intent( in), dimension(isd:ied+1, jsd:jed    ) :: v
    real,    intent( in), dimension(isd:ied,   jsd:jed    ) :: ua, va
    real,    intent(out), dimension(isd:ied+1, jsd:jed+1  ) :: divg_d

    ! local
    real    :: uf(is-2:ie+2, js-1:je+2)
    real    :: vf(is-1:ie+2, js-2:je+2)
    integer :: i, j
    integer :: is2, ie1
    integer :: ret

#ifdef ENABLE_GPTL
  if (do_profile == 1) then
    ret = gptlstart('divergence_corner')
  end if
#endif

    is2 = max(2, is)
    ie1 = min(npx-1, ie+1)

    !     9---4---8
    !     |       |
    !     1   5   3
    !     |       |
    !     6---2---7
    do j = js, je+1
      if ( j == 1 .or. j == npy ) then
        do i = is-1, ie+1
          uf(i, j) = u(i, j) * dyc(i, j) * 0.5 * (sin_sg(i, j-1, 4) + sin_sg(i, j, 2))
        enddo
      else
        do i = is-1, ie+1
          uf(i, j) = (u(i, j) - 0.25 * (va(i, j-1) + va(i, j)) * &
                     (cos_sg(i, j-1, 4) + cos_sg(i, j, 2))) *    &
                      dyc(i, j) * 0.5 * (sin_sg(i, j-1, 4) + sin_sg(i, j, 2))
        enddo
      endif
    enddo

    do j = js-1, je+1
      do i = is2, ie1
        vf(i, j) = (v(i, j) - 0.25 * (ua(i-1, j) + ua(i, j)) * &
                   (cos_sg(i-1, j, 3) + cos_sg(i, j, 1))) * &
                    dxc(i, j) * 0.5 * (sin_sg(i-1, j, 3) + sin_sg(i, j, 1))
      enddo
      if (  is    ==  1  ) then
        vf(1, j) = v(1, j) * dxc(1, j) * 0.5 * (sin_sg(0, j, 3) + &
                   sin_sg(1, j, 1))
      endif
      if ( (ie+1) == npx ) then
        vf(npx, j) = v(npx, j) * dxc(npx, j) * 0.5 * (sin_sg(npx-1, j, 3) + &
                     sin_sg(npx, j, 1))
      endif
    enddo

    do j = js, je+1
      do i = is, ie+1
        divg_d(i, j) = vf(i, j-1) - vf(i, j) + uf(i-1, j) - uf(i, j)
      enddo
    enddo

    ! Remove the extra term at the corners:
    if ( sw_corner ) divg_d(1,     1) = divg_d(1,     1) - vf(1,     0)
    if ( se_corner ) divg_d(npx,   1) = divg_d(npx,   1) - vf(npx,   0)
    if ( ne_corner ) divg_d(npx, npy) = divg_d(npx, npy) + vf(npx, npy)
    if ( nw_corner ) divg_d(1,   npy) = divg_d(1,   npy) + vf(1,   npy)

    do j = js, je+1
      do i = is, ie+1
        divg_d(i, j) = rarea_c(i, j) * divg_d(i, j)
      enddo
    enddo

#ifdef ENABLE_GPTL
  if (do_profile == 1) then
     ret = gptlstop('divergence_corner')
  end if
#endif

  end subroutine divergence_corner


  !------------------------------------------------------------------
  ! d2a2c_vect
  !
  ! There is a limit to how far this routine can fill uc and vc in the
  ! halo, and so either mpp_update_domains or some sort of boundary
  ! routine (extrapolation, outflow, interpolation from a nested grid)
  ! is needed after c_sw is completed if these variables are needed
  ! in the halo
  !------------------------------------------------------------------
  subroutine d2a2c_vect(sw_corner, se_corner, ne_corner, nw_corner,     &
                        sin_sg, cosa_u, cosa_v, cosa_s, rsin_u, rsin_v, &
                        rsin2, dxa, dya, u, v, ua, va, uc, vc, ut, vt)

    logical, intent( in) :: sw_corner, se_corner, ne_corner, nw_corner
    real,    intent( in), dimension(isd:ied,   jsd:jed, 9) :: sin_sg
    real,    intent( in), dimension(isd:ied+1, jsd:jed   ) :: cosa_u, rsin_u
    real,    intent( in), dimension(isd:ied,   jsd:jed+1 ) :: cosa_v, rsin_v
    real,    intent( in), dimension(isd:ied,   jsd:jed   ) :: cosa_s, rsin2
    real,    intent( in), dimension(isd:ied,   jsd:jed   ) :: dxa, dya
    real,    intent( in), dimension(isd:ied,   jsd:jed+1 ) :: u
    real,    intent( in), dimension(isd:ied+1, jsd:jed   ) :: v
    real,    intent(out), dimension(isd:ied+1, jsd:jed   ) :: uc
    real,    intent(out), dimension(isd:ied,   jsd:jed+1 ) :: vc
    real,    intent(out), dimension(isd:ied,   jsd:jed   ) :: ua, va, ut, vt

    ! Local
    real, dimension(isd:ied, jsd:jed) :: utmp, vtmp
    integer :: i, j, ifirst, ilast
    integer :: ret

#ifdef ENABLE_GPTL
  if (do_profile == 1) then
     ret = gptlstart ('d2a2c_vect')
  end if
#endif

    ! Initialize the non-existing corner regions
    utmp = big_number
    vtmp = big_number

    !----------
    ! Interior:
    !----------

    do j = max(4, js-1), min(npy-4, je+1)
      do i = max(4, isd), min(npx-4, ied)
        utmp(i, j) = a2 * (u(i, j-1) + u(i, j+2)) + a1 * (u(i, j) + u(i, j+1))
      enddo
    enddo
    do j = max(4, jsd), min(npy-4, jed)
      do i = max(4, is-1), min(npx-4, ie+1)
        vtmp(i, j) = a2 * (v(i-1, j) + v(i+2, j)) + a1 * (v(i, j) + v(i+1, j))
      enddo
    enddo

    !----------
    ! edges:
    !----------
    if ( js == 1 .or. jsd < 4) then
      do j = jsd, 3
        do i = isd, ied
          utmp(i, j) = 0.5 * (u(i, j) + u(i, j+1))
          vtmp(i, j) = 0.5 * (v(i, j) + v(i+1, j))
        enddo
      enddo
    endif

    if ( (je + 1) == npy .or. jed >= (npy - 4) ) then
      do j = npy-3, jed
        do i = isd, ied
          utmp(i, j) = 0.5 * (u(i, j) + u(i, j+1))
          vtmp(i, j) = 0.5 * (v(i, j) + v(i+1,j) )
        enddo
      enddo
    endif

    if ( is == 1 .or. isd < 4 ) then
      do j = max(4, jsd), min(npy-4, jed)
        do i = isd, 3
          utmp(i, j) = 0.5 * (u(i, j) + u(i, j+1))
          vtmp(i, j) = 0.5 * (v(i, j) + v(i+1, j))
        enddo
      enddo
    endif

    if ( (ie+1) == npx .or. ied >= (npx-4) ) then
      do j = max(4, jsd), min(npy-4, jed)
        do i = npx-3, ied
          utmp(i, j) = 0.5 * (u(i, j) + u(i, j+1))
          vtmp(i, j) = 0.5 * (v(i, j) + v(i+1, j))
        enddo
      enddo
    endif

    do j = js-2, je+2
      do i = is-2, ie+2
        ua(i, j) = (utmp(i, j) - vtmp(i, j) * cosa_s(i, j)) * rsin2(i, j)
        va(i, j) = (vtmp(i, j) - utmp(i, j) * cosa_s(i, j)) * rsin2(i, j)
      enddo
    enddo

    ! A -> C
    !--------------
    ! Fix the edges
    !--------------
    ! Xdir:
    if ( sw_corner ) then
      do i = -2, 0
        utmp(i, 0) = -vtmp(0, 1-i)
      enddo
    endif
    if ( se_corner ) then
      do i = 0, 2
        utmp(npx+i, 0) = vtmp(npx, i+1)
      enddo
    endif
    if ( ne_corner ) then
      do i = 0, 2
        utmp(npx+i, npy) = -vtmp(npx, je-i)
      enddo
    endif
    if ( nw_corner ) then
      do i = -2, 0
        utmp(i, npy) = vtmp(0, je+i)
      enddo
    endif

    ifirst = max(3,     is-1)
    ilast  = min(npx-2, ie+2)
    !---------------------------------------------
    ! 4th order interpolation for interior points:
    !---------------------------------------------
    do j = js-1, je+1
      do i = ifirst, ilast
        uc(i, j) = a1 * (utmp(i-1, j) + utmp(i, j)) + a2 * (utmp(i-2, j) + &
                   utmp(i+1, j))
        ut(i, j) = (uc(i, j) - v(i, j) * cosa_u(i, j)) * rsin_u(i, j)
      enddo
    enddo

    ! Xdir:
    if ( sw_corner ) then
      ua(-1, 0) = -va(0, 2)
      ua( 0, 0) = -va(0, 1)
    endif
    if ( se_corner ) then
      ua(npx,   0) = va(npx, 1)
      ua(npx+1, 0) = va(npx, 2)
    endif
    if ( ne_corner ) then
      ua(npx,   npy) = -va(npx, npy-1)
      ua(npx+1, npy) = -va(npx, npy-2)
    endif
    if ( nw_corner ) then
      ua(-1, npy) = va(0, npy-2)
      ua( 0, npy) = va(0, npy-1)
    endif

    if ( is == 1 ) then
      do j = js-1, je+1
        uc(0, j) = c1 * utmp(-2, j) + c2 * utmp(-1, j) + c3 * utmp(0, j)
        ut(1, j) = edge_interpolate4(ua(-1:2, j), dxa(-1:2, j))
        !Want to use the UPSTREAM value
        if (ut(1, j) < 0.) then
          uc(1, j) = ut(1, j) * sin_sg(0, j, 3)
        else
          uc(1, j) = ut(1, j) * sin_sg(1, j, 1)
        end if
        uc(2, j) = c1 * utmp(3, j) + c2 * utmp(2, j) + c3 * utmp(1, j)
        ut(0, j) = (uc(0, j) - v(0, j) * cosa_u(0, j)) * rsin_u(0, j)
        ut(2, j) = (uc(2, j) - v(2, j) * cosa_u(2, j)) * rsin_u(2, j)
      enddo

      if ( (ie+1) == npx ) then
        do j = js-1, je+1
          uc(npx-1, j) = c1 * utmp(npx-3, j) + c2 * utmp(npx-2, j) + c3 * utmp(npx-1, j)
          i = npx
          ut(i, j) = 0.25 * (-ua(i-2, j) + 3. * (ua(i-1, j) + ua(i, j)) - ua(i+1, j))
          ut(i, j) = edge_interpolate4(ua(i-2:i+1, j), dxa(i-2:i+1, j))
          if ( ut(i,j) < 0. ) then
            uc(i, j) = ut(i, j) * sin_sg(i-1, j, 3)
          else
            uc(i, j) = ut(i, j) * sin_sg(i, j, 1)
          end if
          uc(npx+1, j) = c3 * utmp(npx, j) + c2 * utmp(npx+1, j) + c1 * utmp(npx+2, j)
          ut(npx-1, j) = (uc(npx-1, j) - v(npx-1, j) * cosa_u(npx-1, j)) * rsin_u(npx-1, j)
          ut(npx+1, j) = (uc(npx+1, j) - v(npx+1, j) * cosa_u(npx+1, j)) * rsin_u(npx+1, j)
        enddo
      endif

    endif

    !------
    ! Ydir:
    !------
    if ( sw_corner ) then
      do j = -2, 0
        vtmp(0, j) = -utmp(1-j, 0)
      enddo
    endif
    if ( nw_corner ) then
      do j = 0, 2
        vtmp(0, npy+j) = utmp(j+1, npy)
      enddo
    endif
    if ( se_corner ) then
      do j = -2, 0
        vtmp(npx, j) = utmp(ie+j, 0)
      enddo
    endif
    if ( ne_corner ) then
      do j = 0, 2
        vtmp(npx, npy+j) = -utmp(ie-j, npy)
      enddo
    endif
    if ( sw_corner ) then
      va(0, -1) = -ua(2, 0)
      va(0,  0) = -ua(1, 0)
    endif
    if ( se_corner ) then
      va(npx,  0) = ua(npx-1, 0)
      va(npx, -1) = ua(npx-2, 0)
    endif
    if ( ne_corner ) then
      va(npx, npy  ) = -ua(npx-1, npy)
      va(npx, npy+1) = -ua(npx-2, npy)
    endif
    if ( nw_corner ) then
      va(0, npy)   = ua(1, npy)
      va(0, npy+1) = ua(2, npy)
    endif

    do j = js-1, je+2
      if ( j == 1 ) then
        do i = is-1, ie+1
          vt(i, j) = edge_interpolate4(va(i, -1:2), dya(i, -1:2))
          if ( vt(i, j) < 0. ) then
            vc(i, j) = vt(i, j) * sin_sg(i, j-1, 4)
          else
            vc(i, j) = vt(i, j) * sin_sg(i, j, 2)
          end if
        enddo
      elseif ( j == 0 .or. j == (npy-1) ) then
        do i = is-1, ie+1
          vc(i, j) = c1 * vtmp(i, j-2) + c2 * vtmp(i, j-1) + c3 * vtmp(i, j)
          vt(i, j) = (vc(i, j) - u(i, j) * cosa_v(i, j)) * rsin_v(i, j)
        enddo
      elseif ( j == 2 .or. j == (npy+1) ) then
        do i = is-1, ie+1
          vc(i, j) = c1 * vtmp(i, j+1) + c2 * vtmp(i, j) + c3 * vtmp(i, j-1)
          vt(i, j) = (vc(i, j) - u(i, j) * cosa_v(i, j)) * rsin_v(i, j)
        enddo
      elseif ( j == npy ) then
        do i = is-1, ie+1
          vt(i, j) = 0.25 * (-va(i, j-2) + 3. * (va(i, j-1) + va(i, j)) - va(i, j+1))
          vt(i, j) = edge_interpolate4(va(i, j-2:j+1), dya(i, j-2:j+1))
          if ( vt(i, j) < 0. ) then
            vc(i, j) = vt(i, j) * sin_sg(i, j-1, 4)
          else
            vc(i, j) = vt(i, j) * sin_sg(i, j, 2)
          end if
        enddo
      else
        ! 4th order interpolation for interior points:
        do i = is-1, ie+1
          vc(i, j) = a2 * (vtmp(i, j-2) + vtmp(i, j+1)) + a1 * (vtmp(i, j-1) + vtmp(i, j))
          vt(i, j) = (vc(i, j) - u(i, j) * cosa_v(i, j)) * rsin_v(i, j)
        enddo
      endif
    enddo

#ifdef ENABLE_GPTL
  if (do_profile == 1) then
     ret = gptlstop ('d2a2c_vect')
  end if
#endif

  end subroutine d2a2c_vect


  !------------------------------------------------------------------
  ! edge_interpolate4
  !------------------------------------------------------------------
  real function edge_interpolate4(ua, dxa)

    real, intent(in) :: ua(4)
    real, intent(in) :: dxa(4)

    real    :: u0L, u0R

    u0L = 0.5 * ((2. * dxa(2) + dxa(1)) * ua(2) - dxa(2) * ua(1)) / (dxa(1) + dxa(2))
    u0R = 0.5 * ((2. * dxa(3) + dxa(4)) * ua(3) - dxa(3) * ua(4)) / (dxa(3) + dxa(4))
    edge_interpolate4 = u0L + u0R

    ! This is the original edge-interpolation code, which makes
    ! a relatively small increase in the error in unstretched case 2.
    !
    ! edge_interpolate4 = 0.25 * (3 * (ua(2) + ua(3)) - (ua(1) + ua(4)))

  end function edge_interpolate4


  !------------------------------------------------------------------
  ! fill2_4corners
  !
  ! This routine fill the 4 corners of the scalar fileds only as needed by c_core
  !------------------------------------------------------------------
  subroutine fill2_4corners(q1, q2, dir, sw_corner, se_corner, ne_corner, nw_corner)

 
    integer, intent(   in) :: dir  ! 1: x-dir; 2: y-dir
    real,    intent(inout) :: q1(isd:ied, jsd:jed)
    real,    intent(inout) :: q2(isd:ied, jsd:jed)
    logical, intent(   in) :: sw_corner, se_corner, ne_corner, nw_corner

     integer :: ret

#ifdef ENABLE_GPTL
  if (do_profile == 1) then
    ret = gptlstart('fill2_4corners')
  end if
#endif

    select case(dir)
      case(1)
        if ( sw_corner ) then
          q1(-1, 0) = q1(0, 2)
          q1( 0, 0) = q1(0, 1)
          q2(-1, 0) = q2(0, 2)
          q2( 0, 0) = q2(0, 1)
        endif
        if ( se_corner ) then
          q1(npx+1, 0) = q1(npx, 2)
          q1(npx,   0) = q1(npx, 1)
          q2(npx+1, 0) = q2(npx, 2)
          q2(npx,   0) = q2(npx, 1)
        endif
        if ( nw_corner ) then
          q1( 0, npy) = q1(0, npy-1)
          q1(-1, npy) = q1(0, npy-2)
          q2( 0, npy) = q2(0, npy-1)
          q2(-1, npy) = q2(0, npy-2)
        endif
        if ( ne_corner ) then
          q1(npx,   npy) = q1(npx, npy-1)
          q1(npx+1, npy) = q1(npx, npy-2)
          q2(npx,   npy) = q2(npx, npy-1)
          q2(npx+1, npy) = q2(npx, npy-2)
        endif

      case(2)
        if ( sw_corner ) then
          q1(0,  0) = q1(1, 0)
          q1(0, -1) = q1(2, 0)
          q2(0,  0) = q2(1, 0)
          q2(0, -1) = q2(2, 0)
        endif
        if ( se_corner ) then
          q1(npx,  0) = q1(npx-1, 0)
          q1(npx, -1) = q1(npx-2, 0)
          q2(npx,  0) = q2(npx-1, 0)
          q2(npx, -1) = q2(npx-2, 0)
        endif
        if ( nw_corner ) then
          q1(0, npy)   = q1(1, npy)
          q1(0, npy+1) = q1(2, npy)
          q2(0, npy)   = q2(1, npy)
          q2(0, npy+1) = q2(2, npy)
        endif
        if ( ne_corner ) then
          q1(npx, npy)   = q1(npx-1, npy)
          q1(npx, npy+1) = q1(npx-2, npy)
          q2(npx, npy)   = q2(npx-1, npy)
          q2(npx, npy+1) = q2(npx-2, npy)
        endif

    end select

#ifdef ENABLE_GPTL
  if (do_profile == 1) then
     ret = gptlstop('fill2_4corners')
  end if
#endif

  end subroutine fill2_4corners


  !------------------------------------------------------------------
  ! fill_4corners
  !
  ! This routine fill the 4 corners of the scalar fileds only as needed by c_core
  !------------------------------------------------------------------
  subroutine fill_4corners(q, dir, sw_corner, se_corner, ne_corner, nw_corner)

    integer, intent(   in) :: dir     ! 1: x-dir; 2: y-dir
    real,    intent(inout) :: q(isd:ied, jsd:jed)
    logical, intent(   in) :: sw_corner, se_corner, ne_corner, nw_corner

    integer :: ret
 
#ifdef ENABLE_GPTL
  if (do_profile == 1) then
     ret = gptlstart('fill_4corners')
  end if
#endif

    select case(dir)
      case(1)
        if ( sw_corner ) then
          q(-1, 0) = q(0, 2)
          q( 0, 0) = q(0, 1)
        endif
        if ( se_corner ) then
          q(npx+1, 0) = q(npx, 2)
          q(npx,   0) = q(npx, 1)
        endif
        if ( nw_corner ) then
          q( 0, npy) = q(0, npy-1)
          q(-1, npy) = q(0, npy-2)
        endif
        if ( ne_corner ) then
          q(npx,   npy) = q(npx, npy-1)
          q(npx+1, npy) = q(npx, npy-2)
        endif

      case(2)
        if ( sw_corner ) then
          q(0,  0) = q(1, 0)
          q(0, -1) = q(2, 0)
        endif
        if ( se_corner ) then
          q(npx,  0) = q(npx-1, 0)
          q(npx, -1) = q(npx-2, 0)
        endif
        if ( nw_corner ) then
          q(0, npy  ) = q(1, npy)
          q(0, npy+1) = q(2, npy)
        endif
        if ( ne_corner ) then
          q(npx, npy  ) = q(npx-1, npy)
          q(npx, npy+1) = q(npx-2, npy)
        endif

    end select

#ifdef ENABLE_GPTL
  if (do_profile == 1) then
    ret = gptlstop('fill_4corners')
  end if
#endif

  end subroutine fill_4corners


  !------------------------------------------------------------------
  ! allocate_state
  !
  ! Allocate and initialize the state variables.
  !------------------------------------------------------------------
  subroutine allocate_state

    ! First deallocate state if allocated
    call deallocate_state()

    ! Allocate state arrays
    allocate(rarea  (isd:ied,   jsd:jed       ))
    allocate(rarea_c(isd:ied+1, jsd:jed+1     ))
    allocate(sin_sg (isd:ied,   jsd:jed,     9))
    allocate(cos_sg (isd:ied,   jsd:jed,     9))
    allocate(sina_v (isd:ied,   jsd:jed+1     ))
    allocate(cosa_v (isd:ied,   jsd:jed+1     ))
    allocate(sina_u (isd:ied+1, jsd:jed       ))
    allocate(cosa_u (isd:ied+1, jsd:jed       ))
    allocate(fC     (isd:ied+1, jsd:jed+1     ))
    allocate(rdxc   (isd:ied+1, jsd:jed       ))
    allocate(rdyc   (isd:ied,   jsd:jed+1     ))
    allocate(dx     (isd:ied,   jsd:jed+1     ))
    allocate(dy     (isd:ied+1, jsd:jed       ))
    allocate(dxc    (isd:ied+1, jsd:jed       ))
    allocate(dyc    (isd:ied,   jsd:jed+1     ))
    allocate(cosa_s (isd:ied,   jsd:jed       ))
    allocate(rsin_u (isd:ied+1, jsd:jed       ))
    allocate(rsin_v (isd:ied,   jsd:jed+1     ))
    allocate(rsin2  (isd:ied,   jsd:jed       ))
    allocate(dxa    (isd:ied,   jsd:jed       ))
    allocate(dya    (isd:ied,   jsd:jed       ))
    allocate(delpc  (isd:ied,   jsd:jed,   npz))
    allocate(delp   (isd:ied,   jsd:jed,   npz))
    allocate(ptc    (isd:ied,   jsd:jed,   npz))
    allocate(pt     (isd:ied,   jsd:jed,   npz))
    allocate(u      (isd:ied,   jsd:jed+1, npz))
    allocate(v      (isd:ied+1, jsd:jed,   npz))
    allocate(w      (isd:ied,   jsd:jed,   npz))
    allocate(uc     (isd:ied+1, jsd:jed,   npz))
    allocate(vc     (isd:ied,   jsd:jed+1, npz))
    allocate(ua     (isd:ied,   jsd:jed,   npz))
    allocate(va     (isd:ied,   jsd:jed,   npz))
    allocate(wc     (isd:ied,   jsd:jed,   npz))
    allocate(ut     (isd:ied,   jsd:jed,   npz))
    allocate(vt     (isd:ied,   jsd:jed,   npz))
    allocate(divg_d (isd:ied+1, jsd:jed+1, npz))

    ! Initialize state arrays
    rarea(:,:) = 0.0
    rarea_c(:,:) = 0.0
    sin_sg(:,:,:) = 0.0
    cos_sg(:,:,:) = 0.0
    sina_v(:,:) = 0.0
    cosa_v(:,:) = 0.0
    sina_u(:,:) = 0.0
    cosa_u(:,:) = 0.0
    fC(:,:) = 0.0
    rdxc(:,:) = 0.0
    rdyc(:,:) = 0.0
    dx(:,:) = 0.0
    dy(:,:) = 0.0
    dxc(:,:) = 0.0
    dyc(:,:) = 0.0
    cosa_s(:,:) = 0.0
    rsin_u(:,:) = 0.0
    rsin_v(:,:) = 0.0
    rsin2(:,:) = 0.0
    dxa(:,:) = 0.0
    dya(:,:) = 0.0
    delpc(:,:,:) = 0.0
    delp(:,:,:) = 0.0
    ptc(:,:,:) = 0.0
    pt(:,:,:) = 0.0
    u(:,:,:) = 0.0
    v(:,:,:) = 0.0
    w(:,:,:) = 0.0
    uc(:,:,:) = 0.0
    vc(:,:,:) = 0.0
    ua(:,:,:) = 0.0
    va(:,:,:) = 0.0
    wc(:,:,:) = 0.0
    ut(:,:,:) = 0.0
    vt(:,:,:) = 0.0
    divg_d(:,:,:) = 0.0

  end subroutine allocate_state


  !------------------------------------------------------------------
  ! deallocate_state
  !
  ! Deallocates the state
  !------------------------------------------------------------------
  subroutine deallocate_state()

    ! Deallocate state arrays
    if (allocated(rarea)) then
      deallocate(rarea)
    end if
    if (allocated(rarea_c)) then
      deallocate(rarea_c)
    end if
    if (allocated(sin_sg)) then
      deallocate(sin_sg)
    end if
    if (allocated(cos_sg)) then
      deallocate(cos_sg)
    end if
    if (allocated(sina_v)) then
      deallocate(sina_v)
    end if
    if (allocated(cosa_v)) then
      deallocate(cosa_v)
    end if
    if (allocated(sina_u)) then
      deallocate(sina_u)
    end if
    if (allocated(cosa_u)) then
      deallocate(cosa_u)
    end if
    if (allocated(fC)) then
      deallocate(fC)
    end if
    if (allocated(rdxc)) then
      deallocate(rdxc)
    end if
    if (allocated(rdyc)) then
      deallocate(rdyc)
    end if
    if (allocated(dx)) then
      deallocate(dx)
    end if
    if (allocated(dy)) then
      deallocate(dy)
    end if
    if (allocated(dxc)) then
      deallocate(dxc)
    end if
    if (allocated(dyc)) then
      deallocate(dyc)
    end if
    if (allocated(cosa_s)) then
      deallocate(cosa_s)
    end if
    if (allocated(rsin_u)) then
      deallocate(rsin_u)
    end if
    if (allocated(rsin_v)) then
      deallocate(rsin_v)
    end if
    if (allocated(rsin2)) then
      deallocate(rsin2)
    end if
    if (allocated(dxa)) then
      deallocate(dxa)
    end if
    if (allocated(dya)) then
      deallocate(dya)
    end if
    if (allocated(delpc)) then
      deallocate(delpc)
    end if
    if (allocated(delp)) then
      deallocate(delp)
    end if
    if (allocated(ptc)) then
      deallocate(ptc)
    end if
    if (allocated(pt)) then
      deallocate(pt)
    end if
    if (allocated(u)) then
      deallocate(u)
    end if
    if (allocated(v)) then
      deallocate(v)
    end if
    if (allocated(w)) then
      deallocate(w)
    end if
    if (allocated(uc)) then
      deallocate(uc)
    end if
    if (allocated(vc)) then
      deallocate(vc)
    end if
    if (allocated(ua)) then
      deallocate(ua)
    end if
    if (allocated(va)) then
      deallocate(va)
    end if
    if (allocated(wc)) then
      deallocate(wc)
    end if
    if (allocated(ut)) then
      deallocate(ut)
    end if
    if (allocated(vt)) then
      deallocate(vt)
    end if
    if (allocated(divg_d)) then
      deallocate(divg_d)
    end if

  end subroutine deallocate_state


  !------------------------------------------------------------------
  ! print_state
  !
  ! Prints statistics for the kernel state variables
  !------------------------------------------------------------------
  subroutine print_state(msg)

    character(len=*) :: msg

    write(*,'(A4)') "TEST"
    write(*,'(A5,A115)') "TEST ", repeat("=",115)
    write(*,'(A5,A20)') "TEST ", msg
    write(*,'(A5,A115)') "TEST ", repeat("=",115)
    write(*,'(A5,A15,5A20)') "TEST ", "Variable", "Min", "Max", "First", "Last", "RMS"
    write(*,'(A5,A115)') "TEST ", repeat("-",115)

    call print_2d_variable("rarea", rarea)
    call print_2d_variable("rarea_c", rarea_c)
    call print_3d_variable("sin_sg", sin_sg)
    call print_3d_variable("cos_sg", cos_sg)
    call print_2d_variable("sina_v", sina_v)
    call print_2d_variable("cosa_v", cosa_v)
    call print_2d_variable("sina_u", sina_u)
    call print_2d_variable("cosa_u", cosa_u)
    call print_2d_variable("fC", fC)
    call print_2d_variable("rdxc", rdxc)
    call print_2d_variable("rdyc", rdyc)
    call print_2d_variable("dx", dx)
    call print_2d_variable("dy", dy)
    call print_2d_variable("dxc", dxc)
    call print_2d_variable("dyc", dyc)
    call print_2d_variable("cosa_s", cosa_s)
    call print_2d_variable("rsin_u", rsin_u)
    call print_2d_variable("rsin_v", rsin_v)
    call print_2d_variable("rsin2", rsin2)
    call print_2d_variable("dxa", dxa)
    call print_2d_variable("dya", dya)

    call print_3d_variable("delp", delp)
    call print_3d_variable("delpc", delpc)
    call print_3d_variable("pt", pt)
    call print_3d_variable("ptc", ptc)
    call print_3d_variable("u", u)
    call print_3d_variable("v", v)
    call print_3d_variable("w", w)
    call print_3d_variable("uc", uc)
    call print_3d_variable("vc", vc)
    call print_3d_variable("ua", ua)
    call print_3d_variable("va", va)
    call print_3d_variable("wc", wc)
    call print_3d_variable("ut", ut)
    call print_3d_variable("vt", vt)
    call print_3d_variable("divg_d", divg_d)

    write(*,'(A5,A115)') "TEST ", repeat("-",115)
    write(*,'(A4)') "TEST"

  end subroutine print_state


  !------------------------------------------------------------------
  ! write_state_stats
  !
  ! Writes statistics for the kernel state variables to the given file unit
  !------------------------------------------------------------------
  subroutine write_state_stats(msg, test_file_unit)

    character(len= *),intent(IN) :: msg
    integer          ,intent(IN) :: test_file_unit

    write(test_file_unit,'(A4)') "TEST"
    write(test_file_unit,'(A5,A115)') "TEST ", repeat("=",115)
    write(test_file_unit,'(A5,A20)') "TEST ", msg
    write(test_file_unit,'(A5,A115)') "TEST ", repeat("=",115)
    write(test_file_unit,'(A5,A15,5A20)') "TEST ", "Variable", "Min", "Max", "First", "Last", "RMS"
    write(test_file_unit,'(A5,A115)') "TEST ", repeat("-",115)

    call write_2d_variable(test_file_unit,"rarea", rarea)
    call write_2d_variable(test_file_unit,"rarea_c", rarea_c)
    call write_3d_variable(test_file_unit,"sin_sg", sin_sg)
    call write_3d_variable(test_file_unit,"cos_sg", cos_sg)
    call write_2d_variable(test_file_unit,"sina_v", sina_v)
    call write_2d_variable(test_file_unit,"cosa_v", cosa_v)
    call write_2d_variable(test_file_unit,"sina_u", sina_u)
    call write_2d_variable(test_file_unit,"cosa_u", cosa_u)
    call write_2d_variable(test_file_unit,"fC", fC)
    call write_2d_variable(test_file_unit,"rdxc", rdxc)
    call write_2d_variable(test_file_unit,"rdyc", rdyc)
    call write_2d_variable(test_file_unit,"dx", dx)
    call write_2d_variable(test_file_unit,"dy", dy)
    call write_2d_variable(test_file_unit,"dxc", dxc)
    call write_2d_variable(test_file_unit,"dyc", dyc)
    call write_2d_variable(test_file_unit,"cosa_s", cosa_s)
    call write_2d_variable(test_file_unit,"rsin_u", rsin_u)
    call write_2d_variable(test_file_unit,"rsin_v", rsin_v)
    call write_2d_variable(test_file_unit,"rsin2", rsin2)
    call write_2d_variable(test_file_unit,"dxa", dxa)
    call write_2d_variable(test_file_unit,"dya", dya)

    call write_3d_variable(test_file_unit,"delp", delp)
    call write_3d_variable(test_file_unit,"delpc", delpc)
    call write_3d_variable(test_file_unit,"pt", pt)
    call write_3d_variable(test_file_unit,"ptc", ptc)
    call write_3d_variable(test_file_unit,"u", u)
    call write_3d_variable(test_file_unit,"v", v)
    call write_3d_variable(test_file_unit,"w", w)
    call write_3d_variable(test_file_unit,"uc", uc)
    call write_3d_variable(test_file_unit,"vc", vc)
    call write_3d_variable(test_file_unit,"ua", ua)
    call write_3d_variable(test_file_unit,"va", va)
    call write_3d_variable(test_file_unit,"wc", wc)
    call write_3d_variable(test_file_unit,"ut", ut)
    call write_3d_variable(test_file_unit,"vt", vt)
    call write_3d_variable(test_file_unit,"divg_d", divg_d)

    write(test_file_unit,'(A5,A115)') "TEST ", repeat("-",115)
    write(test_file_unit,'(A4)') "TEST"

  end subroutine write_state_stats


  !------------------------------------------------------------------
  ! print_3d_variable
  !
  ! Prints statistics for a 3d state variable
  !------------------------------------------------------------------
  subroutine print_3d_variable(name, data)

    character(len=*) :: name
    real             :: data(:,:,:)

    ! Note: Assumed shape array sections always start with index=1 for all dimensions
    !       So we don't have to know start/end indices here
    write(*,'(A5,A15,5ES20.10)') "TEST ", name, minval(data), maxval(data), data(1,1,1),  &
                            data(size(data,1), size(data,2), size(data,3)), &
                            sqrt(sum(data**2) / size(data))

  end subroutine print_3d_variable

  !------------------------------------------------------------------
  ! write_3d_variable
  !
  ! Writes statistics for a 3d state variable to the given file unit
  !------------------------------------------------------------------
  subroutine write_3d_variable(unit, name, data)

    integer         ,intent(IN) :: unit
    character(len=*),intent(IN) :: name
    real            ,intent(IN) :: data(:,:,:)

    ! Note: Assumed shape array sections always start with index=1 for all dimensions
    !       So we don't have to know start/end indices here
    write(unit,'(A5, A15,5ES20.10)') "TEST ", name, minval(data), maxval(data), data(1,1,1),  &
                                             data(size(data,1), size(data,2), size(data,3)), &
                                             sqrt(sum(data**2) / size(data))

  end subroutine write_3d_variable

  !------------------------------------------------------------------
  ! print_2d_variable
  !
  ! Prints statistics for a 2d state variable
  !------------------------------------------------------------------
  subroutine print_2d_variable(name, data)

    character(len=*) :: name
    real             :: data(:,:)

    ! Note: Assumed shape array sections always start with index=1 for all dimensions
    !       So we don't have to know start/end indices here
    write(*,'(A5, A15,5ES20.10)') "TEST ", name, minval(data), maxval(data), data(1,1), &
                            data(size(data,1), size(data,2)),            &
                            sqrt(sum(data**2) / size(data))

  end subroutine print_2d_variable


  !------------------------------------------------------------------
  ! write_2d_variable
  !
  ! Writes statistics for a 2d state variable to the given file unit
  !------------------------------------------------------------------
  subroutine write_2d_variable(unit, name, data)

    integer         ,intent(IN) :: unit
    character(len=*),intent(IN) :: name
    real            ,intent(IN) :: data(:,:)

    ! Note: Assumed shape array sections always start with index=1 for all dimensions
    !       So we don't have to know start/end indices here
    write(unit,'(A5, A15,5ES20.10)') "TEST ", name, minval(data), maxval(data), data(1,1), &
                                             data(size(data,1), size(data,2)),            &
                                             sqrt(sum(data**2) / size(data))

  end subroutine write_2d_variable

  !------------------------------------------------------------------
  ! read_state
  !
  ! Read state from NetCDF file
  !------------------------------------------------------------------
  subroutine read_state(filename)

    character(len=*), intent(in) :: filename

    ! netCDF variables
    integer :: ncFileID

    ! Local variables
    integer :: ni, nj, nip1, njp1, nk, nk9

    ! Open file for read only
    call open_file(filename, "r", ncFileID)

    ! Read global attributes
    call read_global_int(ncFileID, "isd", isd)
    call read_global_int(ncFileID, "ied", ied)
    call read_global_int(ncFileID, "jsd", jsd)
    call read_global_int(ncFileID, "jed", jed)
    call read_global_int(ncFileID, "is", is)
    call read_global_int(ncFileID, "ie", ie)
    call read_global_int(ncFileID, "js", js)
    call read_global_int(ncFileID, "je", je)
    call read_global_int(ncFileID, "nord", nord)
    call read_global_int(ncFileID, "npx", npx)
    call read_global_int(ncFileID, "npy", npy)
    call read_global_int(ncFileID, "npz", npz)
    call read_global_real(ncFileID, "dt2", dt2)
    call read_global_logical(ncFileID, "sw_corner", sw_corner)
    call read_global_logical(ncFileID, "se_corner", se_corner)
    call read_global_logical(ncFileID, "nw_corner", nw_corner)
    call read_global_logical(ncFileID, "ne_corner", ne_corner)

    ! Read the model dimensions
    call read_dimension(ncFileID, "ni", ni)
    call read_dimension(ncFileID, "nip1", nip1)
    call read_dimension(ncFileID, "nj", nj)
    call read_dimension(ncFileID, "njp1", njp1)
    call read_dimension(ncFileID, "nk", nk)
    call read_dimension(ncFileID, "nk9", nk9)

    ! Check to make sure state dimensions matches state indices
    if ((ni   /= (ied-isd+1)) .OR. (nj   /= (jed-jsd+1)) .OR. &
        (nip1 /= (ied-isd+2)) .OR. (njp1 /= (jed-jsd+2)) .OR. &
        (nk   /= npz)         .OR. (nk9  /= 9)) then
      write(*,*) "Dimensions and indices of input data are inconsistent"
      stop 1
    end if

    ! Allocate and initialize the state
    call allocate_state()

    ! Read the state variables
    call read_2d_real(ncFileID, "rarea", rarea)
    call read_2d_real(ncFileID, "rarea_c", rarea_c)
    call read_3d_real(ncFileID, "sin_sg", sin_sg)
    call read_3d_real(ncFileID, "cos_sg", cos_sg)
    call read_2d_real(ncFileID, "sina_v", sina_v)
    call read_2d_real(ncFileID, "cosa_v", cosa_v)
    call read_2d_real(ncFileID, "sina_u", sina_u)
    call read_2d_real(ncFileID, "cosa_u", cosa_u)
    call read_2d_real(ncFileID, "fC", fC)
    call read_2d_real(ncFileID, "rdxc", rdxc)
    call read_2d_real(ncFileID, "rdyc", rdyc)
    call read_2d_real(ncFileID, "dx", dx)
    call read_2d_real(ncFileID, "dy", dy)
    call read_2d_real(ncFileID, "dxc", dxc)
    call read_2d_real(ncFileID, "dyc", dyc)
    call read_2d_real(ncFileID, "cosa_s", cosa_s)
    call read_2d_real(ncFileID, "rsin_u", rsin_u)
    call read_2d_real(ncFileID, "rsin_v", rsin_v)
    call read_2d_real(ncFileID, "rsin2", rsin2)
    call read_2d_real(ncFileID, "dxa", dxa)
    call read_2d_real(ncFileID, "dya", dya)
    call read_3d_real(ncFileID, "delp", delp)
    call read_3d_real(ncFileID, "delpc", delpc)
    call read_3d_real(ncFileID, "pt", pt)
    call read_3d_real(ncFileID, "ptc", ptc)
    call read_3d_real(ncFileID, "u", u)
    call read_3d_real(ncFileID, "v", v)
    call read_3d_real(ncFileID, "w", w)
    call read_3d_real(ncFileID, "uc", uc)
    call read_3d_real(ncFileID, "vc", vc)
    call read_3d_real(ncFileID, "ua", ua)
    call read_3d_real(ncFileID, "va", va)
    call read_3d_real(ncFileID, "wc", wc)
    call read_3d_real(ncFileID, "ut", ut)
    call read_3d_real(ncFileID, "vt", vt)
    call read_3d_real(ncFileID, "divg_d", divg_d)

    ! Close the NetCDF file
    call close_file(ncFileID)

  end subroutine read_state


  !------------------------------------------------------------------
  ! write_state
  !
  ! Write state to NetCDF file
  !------------------------------------------------------------------
  subroutine write_state(filename)

    character(len=*), intent(in) :: filename

    ! General netCDF variables
    integer :: ncFileID
    integer :: niDimID, njDimID, nip1DimID, njp1DimID, nkDimID, nk9DimID
    integer :: rareaVarID, rarea_cVarID, sin_sgVarID, cos_sgVarID, sina_vVarID
    integer :: cosa_vVarID, sina_uVarID, cosa_uVarID, fCVarID, rdxcVarID
    integer :: rdycVarID, dxVarID, dyVarID, dxcVarID, dycVarID, cosa_sVarID
    integer :: rsin_uVarID, rsin_vVarID, rsin2VarID, dxaVarID, dyaVarID
    integer :: delpVarID, delpcVarID, ptVarID, ptcVarID, uVarID, vVarID, wVarID
    integer :: ucVarID, vcVarID, uaVarID, vaVarID, wcVarID, utVarID, vtVarID
    integer :: divg_dVarID

    ! Local variables
    character(len=8)      :: crdate  ! Needed by F90 DATE_AND_TIME intrinsic
    character(len=10)     :: crtime  ! Needed by F90 DATE_AND_TIME intrinsic
    character(len=5)      :: crzone  ! Needed by F90 DATE_AND_TIME intrinsic
    integer, dimension(8) :: values  ! Needed by F90 DATE_AND_TIME intrinsic
    character(len=19)     :: timestr ! String representation of clock

    ! Open new file, overwriting previous contents
    call open_file(filename, "w", ncFileID)

    ! Write Global Attributes
    call DATE_AND_TIME(crdate,crtime,crzone,values)
    write(timestr,'(i4,2(a,i2.2),1x,i2.2,2(a,i2.2))') &
          values(1), '/', values(2), '/', values(3), values(5), ':', &
          values(6), ':', values(7)
    call write_global_character(ncFileID, "creation_date", timestr)
    call write_global_character(ncFileID, "kernel_name", "c_sw")
    call write_global_int(ncFileID, "isd", isd)
    call write_global_int(ncFileID, "ied", ied)
    call write_global_int(ncFileID, "jsd", jsd)
    call write_global_int(ncFileID, "jed", jed)
    call write_global_int(ncFileID, "is", is)
    call write_global_int(ncFileID, "ie", ie)
    call write_global_int(ncFileID, "js", js)
    call write_global_int(ncFileID, "je", je)
    call write_global_int(ncFileID, "nord", nord)
    call write_global_int(ncFileID, "npx", npx)
    call write_global_int(ncFileID, "npy", npy)
    call write_global_int(ncFileID, "npz", npz)
    call write_global_real(ncFileID, "dt2", dt2)
    call write_global_logical(ncFileID, "sw_corner", sw_corner)
    call write_global_logical(ncFileID, "se_corner", se_corner)
    call write_global_logical(ncFileID, "nw_corner", nw_corner)
    call write_global_logical(ncFileID, "ne_corner", ne_corner)

    ! Define the i,j dimensions
    call define_dim(ncFileID, "ni", ied-isd+1, niDimID)
    call define_dim(ncFileID, "nj", jed-jsd+1, njDimID)

    ! Define the i+1, j+1 dimensions
    call define_dim(ncFileID, "nip1", ied-isd+2, nip1DimID)
    call define_dim(ncFileID, "njp1", jed-jsd+2, njp1DimID)

    ! Define the k dimension
    call define_dim(ncFileID, "nk", npz, nkDimID)
    call define_dim(ncFileID, "nk9", 9, nk9DimID)

    ! Define the fields
    call define_var_2d_real(ncFileID, "rarea",   niDimID,   njDimID,             rareaVarID)
    call define_var_2d_real(ncFileID, "rarea_c", nip1DimID, njp1DimID,           rarea_cVarID)
    call define_var_3d_real(ncFileID, "sin_sg",  niDimID,   njDimID,   nk9DimID, sin_sgVarID)
    call define_var_3d_real(ncFileID, "cos_sg",  niDimID,   njDimID,   nk9DimID, cos_sgVarID)
    call define_var_2d_real(ncFileID, "sina_v",  niDimID,   njp1DimID,           sina_vVarID)
    call define_var_2d_real(ncFileID, "cosa_v",  niDimID,   njp1DimID,           cosa_vVarID)
    call define_var_2d_real(ncFileID, "sina_u",  nip1DimID, njDimID,             sina_uVarID)
    call define_var_2d_real(ncFileID, "cosa_u",  nip1DimID, njDimID,             cosa_uVarID)
    call define_var_2d_real(ncFileID, "fC",      nip1DimID, njp1DimID,           fCVarID)
    call define_var_2d_real(ncFileID, "rdxc",    nip1DimID, njDimID,             rdxcVarID)
    call define_var_2d_real(ncFileID, "rdyc",    niDimID,   njp1DimID,           rdycVarID)
    call define_var_2d_real(ncFileID, "dx",      niDimID,   njp1DimID,           dxVarID)
    call define_var_2d_real(ncFileID, "dy",      nip1DimID, njDimID,             dyVarID)
    call define_var_2d_real(ncFileID, "dxc",     nip1DimID, njDimID,             dxcVarID)
    call define_var_2d_real(ncFileID, "dyc",     niDimID,   njp1DimID,           dycVarID)
    call define_var_2d_real(ncFileID, "cosa_s",  niDimID,   njDimID,             cosa_sVarID)
    call define_var_2d_real(ncFileID, "rsin_u",  nip1DimID, njDimID,             rsin_uVarID)
    call define_var_2d_real(ncFileID, "rsin_v",  niDimID,   njp1DimID,           rsin_vVarID)
    call define_var_2d_real(ncFileID, "rsin2",   niDimID,   njDimID,             rsin2VarID)
    call define_var_2d_real(ncFileID, "dxa",     niDimID,   njDimID,             dxaVarID)
    call define_var_2d_real(ncFileID, "dya",     niDimID,   njDimID,             dyaVarID)
    call define_var_3d_real(ncFileID, "delp",    niDimID,   njDimID,   nkDimID,  delpVarID)
    call define_var_3d_real(ncFileID, "delpc",   niDimID,   njDimID,   nkDimID,  delpcVarID)
    call define_var_3d_real(ncFileID, "pt",      niDimID,   njDimID,   nkDimID,  ptVarID)
    call define_var_3d_real(ncFileID, "ptc",     niDimID,   njDimID,   nkDimID,  ptcVarID)
    call define_var_3d_real(ncFileID, "u",       niDimID,   njp1DimID, nkDimID,  uVarID)
    call define_var_3d_real(ncFileID, "v",       nip1DimID, njDimID,   nkDimID,  vVarID)
    call define_var_3d_real(ncFileID, "w",       niDimID,   njDimID,   nkDimID,  wVarID)
    call define_var_3d_real(ncFileID, "uc",      nip1DimID, njDimID,   nkDimID,  ucVarID)
    call define_var_3d_real(ncFileID, "vc",      niDimID,   njp1DimID, nkDimID,  vcVarID)
    call define_var_3d_real(ncFileID, "ua",      niDimID,   njDimID,   nkDimID,  uaVarID)
    call define_var_3d_real(ncFileID, "va",      niDimID,   njDimID,   nkDimID,  vaVarID)
    call define_var_3d_real(ncFileID, "wc",      niDimID,   njDimID,   nkDimID,  wcVarID)
    call define_var_3d_real(ncFileID, "ut",      niDimID,   njDimID,   nkDimID,  utVarID)
    call define_var_3d_real(ncFileID, "vt",      niDimID,   njDimID,   nkDimID,  vtVarID)
    call define_var_3d_real(ncFileID, "divg_d",  nip1DimID, njp1DimID, nkDimID,  divg_dVarID)

    ! Leave define mode so we can fill
    call define_off(ncFileID)

    ! Fill the variables
    call write_var_2d_real(ncFileID, rareaVarID, rarea)
    call write_var_2d_real(ncFileID, rarea_cVarID, rarea_c)
    call write_var_3d_real(ncFileID, sin_sgVarID, sin_sg)
    call write_var_3d_real(ncFileID, cos_sgVarID, cos_sg)
    call write_var_2d_real(ncFileID, sina_vVarID, sina_v)
    call write_var_2d_real(ncFileID, cosa_vVarID, cosa_v)
    call write_var_2d_real(ncFileID, sina_uVarID, sina_u)
    call write_var_2d_real(ncFileID, cosa_uVarID, cosa_u)
    call write_var_2d_real(ncFileID, fCVarID, fC)
    call write_var_2d_real(ncFileID, rdxcVarID, rdxc)
    call write_var_2d_real(ncFileID, rdycVarID, rdyc)
    call write_var_2d_real(ncFileID, dxVarID, dx)
    call write_var_2d_real(ncFileID, dyVarID, dy)
    call write_var_2d_real(ncFileID, dxcVarID, dxc)
    call write_var_2d_real(ncFileID, dycVarID, dyc)
    call write_var_2d_real(ncFileID, cosa_sVarID, cosa_s)
    call write_var_2d_real(ncFileID, rsin_uVarID, rsin_u)
    call write_var_2d_real(ncFileID, rsin_vVarID, rsin_v)
    call write_var_2d_real(ncFileID, rsin2VarID, rsin2)
    call write_var_2d_real(ncFileID, dxaVarID, dxa)
    call write_var_2d_real(ncFileID, dyaVarID, dya)
    call write_var_3d_real(ncFileID, delpVarID, delp)
    call write_var_3d_real(ncFileID, delpcVarID, delpc)
    call write_var_3d_real(ncFileID, ptVarID, pt)
    call write_var_3d_real(ncFileID, ptcVarID, ptc)
    call write_var_3d_real(ncFileID, uVarID, u)
    call write_var_3d_real(ncFileID, vVarID, v)
    call write_var_3d_real(ncFileID, wVarID, w)
    call write_var_3d_real(ncFileID, ucVarID, uc)
    call write_var_3d_real(ncFileID, vcVarID, vc)
    call write_var_3d_real(ncFileID, uaVarID, ua)
    call write_var_3d_real(ncFileID, vaVarID, va)
    call write_var_3d_real(ncFileID, wcVarID, wc)
    call write_var_3d_real(ncFileID, utVarID, ut)
    call write_var_3d_real(ncFileID, vtVarID, vt)
    call write_var_3d_real(ncFileID, divg_dVarID, divg_d)

    ! Close the NetCDF file
    call close_file(ncFileID)

  end subroutine write_state


  !------------------------------------------------------------------
  ! write_subdomain
  !
  ! Write a subdomain of the state to NetCDF file
  !------------------------------------------------------------------
  subroutine write_subdomain(filename, i1, j1, ni, nj)

    character(len=*), intent(in) :: filename
    integer,          intent(in) :: i1, j1, ni, nj

    ! General netCDF variables
    integer :: ncFileID
    integer :: niDimID, njDimID, nip1DimID, njp1DimID, nkDimID, nk9DimID
    integer :: rareaVarID, rarea_cVarID, sin_sgVarID, cos_sgVarID, sina_vVarID
    integer :: cosa_vVarID, sina_uVarID, cosa_uVarID, fCVarID, rdxcVarID
    integer :: rdycVarID, dxVarID, dyVarID, dxcVarID, dycVarID, cosa_sVarID
    integer :: rsin_uVarID, rsin_vVarID, rsin2VarID, dxaVarID, dyaVarID
    integer :: delpVarID, delpcVarID, ptVarID, ptcVarID, uVarID, vVarID, wVarID
    integer :: ucVarID, vcVarID, uaVarID, vaVarID, wcVarID, utVarID, vtVarID
    integer :: divg_dVarID

    ! Local variables
    character(len=8)      :: crdate  ! Needed by F90 DATE_AND_TIME intrinsic
    character(len=10)     :: crtime  ! Needed by F90 DATE_AND_TIME intrinsic
    character(len=5)      :: crzone  ! Needed by F90 DATE_AND_TIME intrinsic
    integer, dimension(8) :: values  ! Needed by F90 DATE_AND_TIME intrinsic
    character(len=19)     :: timestr ! String representation of clock

    integer :: sub_isd, sub_ied, sub_jsd, sub_jed
    integer :: sub_is, sub_js, sub_ie, sub_je
    integer :: sub_npx, sub_npy

    ! Validate the subdomain indices
    if ((i1 < is) .OR. (i1 > ie) .OR. &
        (j1 < js) .OR. (j1 > je) .OR. &
        (i1 + ni - 1 > ie)      .OR.  &
        (j1 + nj - 1 > je)) then
      write(*,*) "Cannot write subdomain because it is out of bounds"
      stop 1
    end if

    ! Calculate indices for subdomain (halo size is 3)
    sub_is = i1
    sub_ie = i1 + ni - 1
    sub_js = j1
    sub_je = j1 + nj - 1
    sub_isd = sub_is - 3
    sub_ied = sub_ie + 3
    sub_jsd = sub_js - 3
    sub_jed = sub_je + 3
    sub_npx = sub_ie - sub_is + 2
    sub_npy = sub_je - sub_js + 2

    ! Open new file, overwriting previous contents
    call open_file(filename, "w", ncFileID)

    ! Write Global Attributes
    call DATE_AND_TIME(crdate,crtime,crzone,values)
    write(timestr,'(i4,2(a,i2.2),1x,i2.2,2(a,i2.2))') &
          values(1), '/', values(2), '/', values(3), values(5), ':', &
          values(6), ':', values(7)
    call write_global_character(ncFileID, "creation_date", timestr)
    call write_global_character(ncFileID, "kernel_name", "c_sw")
    call write_global_int(ncFileID, "isd", sub_isd)
    call write_global_int(ncFileID, "ied", sub_ied)
    call write_global_int(ncFileID, "jsd", sub_jsd)
    call write_global_int(ncFileID, "jed", sub_jed)
    call write_global_int(ncFileID, "is",  sub_is)
    call write_global_int(ncFileID, "ie",  sub_ie)
    call write_global_int(ncFileID, "js",  sub_js)
    call write_global_int(ncFileID, "je",  sub_je)
    call write_global_int(ncFileID, "nord", nord)
    call write_global_int(ncFileID, "npx", sub_npx)
    call write_global_int(ncFileID, "npy", sub_npy)
    call write_global_int(ncFileID, "npz", npz)
    call write_global_real(ncFileID, "dt2", dt2)
    if ((sub_is == 1) .AND. (sub_js == 1)) then
      call write_global_logical(ncFileID, "sw_corner", sw_corner)
    else
      call write_global_logical(ncFileID, "sw_corner", .not. sw_corner)
    end if
    if ((sub_npx == npx) .AND. (sub_js == 1)) then
      call write_global_logical(ncFileID, "se_corner", se_corner)
    else
      call write_global_logical(ncFileID, "se_corner", .not. se_corner)
    end if
    if ((sub_is == 1) .AND. (sub_npy == npy)) then
      call write_global_logical(ncFileID, "nw_corner", nw_corner)
    else
      call write_global_logical(ncFileID, "nw_corner", .not. nw_corner)
    end if
    if ((sub_npx == npx) .AND. (sub_npy == npy)) then
      call write_global_logical(ncFileID, "ne_corner", ne_corner)
    else
      call write_global_logical(ncFileID, "ne_corner", .not. ne_corner)
    end if

    ! Define the i,j dimensions
    call define_dim(ncFileID, "ni", sub_ied-sub_isd+1, niDimID)
    call define_dim(ncFileID, "nj", sub_jed-sub_jsd+1, njDimID)

    ! Define the i+1, j+1 dimensions
    call define_dim(ncFileID, "nip1", sub_ied-sub_isd+2, nip1DimID)
    call define_dim(ncFileID, "njp1", sub_jed-sub_jsd+2, njp1DimID)

    ! Define the k dimension
    call define_dim(ncFileID, "nk", npz, nkDimID)
    call define_dim(ncFileID, "nk9", 9, nk9DimID)

    ! Define the fields
    call define_var_2d_real(ncFileID, "rarea",   niDimID,   njDimID,             rareaVarID)
    call define_var_2d_real(ncFileID, "rarea_c", nip1DimID, njp1DimID,           rarea_cVarID)
    call define_var_3d_real(ncFileID, "sin_sg",  niDimID,   njDimID,   nk9DimID, sin_sgVarID)
    call define_var_3d_real(ncFileID, "cos_sg",  niDimID,   njDimID,   nk9DimID, cos_sgVarID)
    call define_var_2d_real(ncFileID, "sina_v",  niDimID,   njp1DimID,           sina_vVarID)
    call define_var_2d_real(ncFileID, "cosa_v",  niDimID,   njp1DimID,           cosa_vVarID)
    call define_var_2d_real(ncFileID, "sina_u",  nip1DimID, njDimID,             sina_uVarID)
    call define_var_2d_real(ncFileID, "cosa_u",  nip1DimID, njDimID,             cosa_uVarID)
    call define_var_2d_real(ncFileID, "fC",      nip1DimID, njp1DimID,           fCVarID)
    call define_var_2d_real(ncFileID, "rdxc",    nip1DimID, njDimID,             rdxcVarID)
    call define_var_2d_real(ncFileID, "rdyc",    niDimID,   njp1DimID,           rdycVarID)
    call define_var_2d_real(ncFileID, "dx",      niDimID,   njp1DimID,           dxVarID)
    call define_var_2d_real(ncFileID, "dy",      nip1DimID, njDimID,             dyVarID)
    call define_var_2d_real(ncFileID, "dxc",     nip1DimID, njDimID,             dxcVarID)
    call define_var_2d_real(ncFileID, "dyc",     niDimID,   njp1DimID,           dycVarID)
    call define_var_2d_real(ncFileID, "cosa_s",  niDimID,   njDimID,             cosa_sVarID)
    call define_var_2d_real(ncFileID, "rsin_u",  nip1DimID, njDimID,             rsin_uVarID)
    call define_var_2d_real(ncFileID, "rsin_v",  niDimID,   njp1DimID,           rsin_vVarID)
    call define_var_2d_real(ncFileID, "rsin2",   niDimID,   njDimID,             rsin2VarID)
    call define_var_2d_real(ncFileID, "dxa",     niDimID,   njDimID,             dxaVarID)
    call define_var_2d_real(ncFileID, "dya",     niDimID,   njDimID,             dyaVarID)
    call define_var_3d_real(ncFileID, "delp",    niDimID,   njDimID,   nkDimID,  delpVarID)
    call define_var_3d_real(ncFileID, "delpc",   niDimID,   njDimID,   nkDimID,  delpcVarID)
    call define_var_3d_real(ncFileID, "pt",      niDimID,   njDimID,   nkDimID,  ptVarID)
    call define_var_3d_real(ncFileID, "ptc",     niDimID,   njDimID,   nkDimID,  ptcVarID)
    call define_var_3d_real(ncFileID, "u",       niDimID,   njp1DimID, nkDimID,  uVarID)
    call define_var_3d_real(ncFileID, "v",       nip1DimID, njDimID,   nkDimID,  vVarID)
    call define_var_3d_real(ncFileID, "w",       niDimID,   njDimID,   nkDimID,  wVarID)
    call define_var_3d_real(ncFileID, "uc",      nip1DimID, njDimID,   nkDimID,  ucVarID)
    call define_var_3d_real(ncFileID, "vc",      niDimID,   njp1DimID, nkDimID,  vcVarID)
    call define_var_3d_real(ncFileID, "ua",      niDimID,   njDimID,   nkDimID,  uaVarID)
    call define_var_3d_real(ncFileID, "va",      niDimID,   njDimID,   nkDimID,  vaVarID)
    call define_var_3d_real(ncFileID, "wc",      niDimID,   njDimID,   nkDimID,  wcVarID)
    call define_var_3d_real(ncFileID, "ut",      niDimID,   njDimID,   nkDimID,  utVarID)
    call define_var_3d_real(ncFileID, "vt",      niDimID,   njDimID,   nkDimID,  vtVarID)
    call define_var_3d_real(ncFileID, "divg_d",  nip1DimID, njp1DimID, nkDimID,  divg_dVarID)

    ! Leave define mode so we can fill
    call define_off(ncFileID)

    ! Fill the variables
    call write_var_2d_real(ncFileID, rareaVarID,     rarea(sub_isd:sub_ied,   sub_jsd:sub_jed     ))
    call write_var_2d_real(ncFileID, rarea_cVarID, rarea_c(sub_isd:sub_ied+1, sub_jsd:sub_jed+1   ))
    call write_var_3d_real(ncFileID, sin_sgVarID,   sin_sg(sub_isd:sub_ied,   sub_jsd:sub_jed,   :))
    call write_var_3d_real(ncFileID, cos_sgVarID,   cos_sg(sub_isd:sub_ied,   sub_jsd:sub_jed,   :))
    call write_var_2d_real(ncFileID, sina_vVarID,   sina_v(sub_isd:sub_ied,   sub_jsd:sub_jed+1   ))
    call write_var_2d_real(ncFileID, cosa_vVarID,   cosa_v(sub_isd:sub_ied,   sub_jsd:sub_jed+1   ))
    call write_var_2d_real(ncFileID, sina_uVarID,   sina_u(sub_isd:sub_ied+1, sub_jsd:sub_jed     ))
    call write_var_2d_real(ncFileID, cosa_uVarID,   cosa_u(sub_isd:sub_ied+1, sub_jsd:sub_jed     ))
    call write_var_2d_real(ncFileID, fCVarID,           fC(sub_isd:sub_ied+1, sub_jsd:sub_jed+1   ))
    call write_var_2d_real(ncFileID, rdxcVarID,       rdxc(sub_isd:sub_ied+1, sub_jsd:sub_jed     ))
    call write_var_2d_real(ncFileID, rdycVarID,       rdyc(sub_isd:sub_ied,   sub_jsd:sub_jed+1   ))
    call write_var_2d_real(ncFileID, dxVarID,           dx(sub_isd:sub_ied,   sub_jsd:sub_jed+1   ))
    call write_var_2d_real(ncFileID, dyVarID,           dy(sub_isd:sub_ied+1, sub_jsd:sub_jed     ))
    call write_var_2d_real(ncFileID, dxcVarID,         dxc(sub_isd:sub_ied+1, sub_jsd:sub_jed     ))
    call write_var_2d_real(ncFileID, dycVarID,         dyc(sub_isd:sub_ied,   sub_jsd:sub_jed+1   ))
    call write_var_2d_real(ncFileID, cosa_sVarID,   cosa_s(sub_isd:sub_ied,   sub_jsd:sub_jed     ))
    call write_var_2d_real(ncFileID, rsin_uVarID,   rsin_u(sub_isd:sub_ied+1, sub_jsd:sub_jed     ))
    call write_var_2d_real(ncFileID, rsin_vVarID,   rsin_v(sub_isd:sub_ied,   sub_jsd:sub_jed+1   ))
    call write_var_2d_real(ncFileID, rsin2VarID,     rsin2(sub_isd:sub_ied,   sub_jsd:sub_jed     ))
    call write_var_2d_real(ncFileID, dxaVarID,         dxa(sub_isd:sub_ied,   sub_jsd:sub_jed     ))
    call write_var_2d_real(ncFileID, dyaVarID,         dya(sub_isd:sub_ied,   sub_jsd:sub_jed     ))
    call write_var_3d_real(ncFileID, delpVarID,       delp(sub_isd:sub_ied,   sub_jsd:sub_jed,   :))
    call write_var_3d_real(ncFileID, delpcVarID,     delpc(sub_isd:sub_ied,   sub_jsd:sub_jed,   :))
    call write_var_3d_real(ncFileID, ptVarID,           pt(sub_isd:sub_ied,   sub_jsd:sub_jed,   :))
    call write_var_3d_real(ncFileID, ptcVarID,         ptc(sub_isd:sub_ied,   sub_jsd:sub_jed,   :))
    call write_var_3d_real(ncFileID, uVarID,             u(sub_isd:sub_ied,   sub_jsd:sub_jed+1, :))
    call write_var_3d_real(ncFileID, vVarID,             v(sub_isd:sub_ied+1, sub_jsd:sub_jed,   :))
    call write_var_3d_real(ncFileID, wVarID,             w(sub_isd:sub_ied,   sub_jsd:sub_jed,   :))
    call write_var_3d_real(ncFileID, ucVarID,           uc(sub_isd:sub_ied+1, sub_jsd:sub_jed,   :))
    call write_var_3d_real(ncFileID, vcVarID,           vc(sub_isd:sub_ied,   sub_jsd:sub_jed+1, :))
    call write_var_3d_real(ncFileID, uaVarID,           ua(sub_isd:sub_ied,   sub_jsd:sub_jed,   :))
    call write_var_3d_real(ncFileID, vaVarID,           va(sub_isd:sub_ied,   sub_jsd:sub_jed,   :))
    call write_var_3d_real(ncFileID, wcVarID,           wc(sub_isd:sub_ied,   sub_jsd:sub_jed,   :))
    call write_var_3d_real(ncFileID, utVarID,           ut(sub_isd:sub_ied,   sub_jsd:sub_jed,   :))
    call write_var_3d_real(ncFileID, vtVarID,           vt(sub_isd:sub_ied,   sub_jsd:sub_jed,   :))
    call write_var_3d_real(ncFileID, divg_dVarID,   divg_d(sub_isd:sub_ied+1, sub_jsd:sub_jed+1, :))

    ! Close the NetCDF file
    call close_file(ncFileID)

  end subroutine write_subdomain

end module sw_core_mod

