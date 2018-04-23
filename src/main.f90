program main
  use bd_types
  implicit none

  type(bdparams_t) :: p
  type(bdvars_t) :: m
  integer(int_t), parameter:: nt = 10000
  integer(int_t) :: i, option

  write(*,*) "Select array layout: "
  write(*,*) "    0: BeamDyn layout"
  write(*,*) "    1: Optimized layout"
  write(*,'(A)', advance="no") "Enter: "
  read(*,*) option

  call init_bdparams(p)
  call init_bdvars(m, p, option)

  print '(4I4)', p%npe, p%ndof, p%nqp, p%nelem
  select case (option)
  case (0)
     do i = 1, nt
        call compute_matrix_orig(m, p)
     end do
  case (1)
     do i = 1, nt
        call compute_matrix(m, p)
     end do
  end select

contains
  !> Cache friendly version of BD_ElementMatrixGA2
  subroutine compute_matrix(m, p)
    use dtypes
    implicit none

    type(bdvars_t), intent(inout):: m
    type(bdparams_t), intent(in):: p

    integer(int_t):: i, j, ii, jj, ie, iq

    ie = 1 ! nelem is always one

    m%elk = 0.0_rdp
    m%elg = 0.0_rdp
    m%elm = 0.0_rdp
    do j = 1, p%npe
       do jj = 1, p%ndof
          do i = 1, p%npe
             do ii = 1, p%ndof
                m%elk(ii, i, jj, j) = dot_product(&
                     (m%qp%Qe(:, ii, jj, ie) + m%qp%Ki(:, ii, jj, ie)), &
                     p%qptw_shp_shp_jac(:, i, j, ie))
             end do
          end do
       end do
    end do

  end subroutine compute_matrix

  !> Mimic original implementation of BD_ElementMatrixGA2
  subroutine compute_matrix_orig(m, p)
    use dtypes
    implicit none

    type(bdvars_t), intent(inout):: m
    type(bdparams_t), intent(in):: p

    integer(int_t):: i, j, ii, jj, ie, iq

    ie = 1 ! nelem is always one

    m%elk = 0.0_rdp
    m%elg = 0.0_rdp
    m%elm = 0.0_rdp
    do j = 1, p%npe
       do jj = 1, p%ndof
          do i = 1, p%npe
             do ii = 1, p%ndof
                do iq = 1, p%nqp
                   m%elk(ii, i, jj, j) = m%elk(ii, i, jj, j) &
                        + (m%qp%Qe(ii, jj, iq, ie) + m%qp%Ki(ii, jj, iq, ie)) &
                          * p%qptw_shp_shp_jac(iq, i, j, ie)
                end do
             end do
          end do
       end do
    end do
  end subroutine compute_matrix_orig
end program main
