
module dtypes
  implicit none

  integer, parameter:: int_t = selected_int_kind(10)
  integer, parameter:: float = selected_real_kind(6, 30)
  integer, parameter:: double = selected_real_kind(14, 300)

  integer, parameter:: rdp = double
end module dtypes

module bd_types
  use dtypes, only: rdp, int_t
  implicit none

  type, public :: bdparams_t
     real(kind=rdp), dimension(:,:,:,:), allocatable:: qptw_shp_shp_jac
     real(kind=rdp), dimension(:,:,:,:), allocatable:: qptw_shp_shpder_jac
     real(kind=rdp), dimension(:,:,:,:), allocatable:: qptw_shpder_shpder_jac
     integer(kind=int_t):: ndof, elemdof, nqp, npe, nelem
  end type bdparams_t

  type, public :: bdeq_t
     real(kind=rdp), dimension(:,:,:,:), allocatable:: Gi, Ki, Mi
     real(kind=rdp), dimension(:,:,:,:), allocatable:: Oe, Pe, Qe
     real(kind=rdp), dimension(:,:,:,:), allocatable:: Gd, Od, Pd, Qd, Sd, Xd, Yd
  end type bdeq_t

  type, public :: bdvars_t
     type(bdeq_t) :: qp
     real(kind=rdp), dimension(:,:,:,:), allocatable:: elk, elg, elm
  end type bdvars_t

contains
  subroutine init_bdparams(p)
    type(bdparams_t), intent(inout) :: p

    p%nelem = 1
    p%ndof = 6
    p%npe = 6
    p%nqp = 49
    allocate(p%qptw_shp_shp_jac(p%nqp, p%npe, p%npe, p%nelem))
    allocate(p%qptw_shp_shpder_jac(p%nqp, p%npe, p%npe, p%nelem))
    allocate(p%qptw_shpder_shpder_jac(p%nqp, p%npe, p%npe, p%nelem))

    call random_seed()
    call random_number(p%qptw_shp_shp_jac)
    call random_number(p%qptw_shp_shpder_jac)
    call random_number(p%qptw_shpder_shpder_jac)
  end subroutine init_bdparams

  subroutine init_bdvars(m, p, option)
    type(bdvars_t), intent(inout) :: m
    type(bdparams_t), intent(inout) :: p
    integer(int_t), intent(in) :: option

    allocate(m%elm(p%ndof, p%npe, p%ndof, p%npe))
    allocate(m%elg(p%ndof, p%npe, p%ndof, p%npe))
    allocate(m%elk(p%ndof, p%npe, p%ndof, p%npe))

    select case (option)
    case (0)
       allocate(m%qp%Gi(6, 6, p%nqp, p%nelem))
       allocate(m%qp%Ki(6, 6, p%nqp, p%nelem))
       allocate(m%qp%Mi(6, 6, p%nqp, p%nelem))
       allocate(m%qp%Oe(6, 6, p%nqp, p%nelem))
       allocate(m%qp%Pe(6, 6, p%nqp, p%nelem))
       allocate(m%qp%Qe(6, 6, p%nqp, p%nelem))
       allocate(m%qp%Gd(6, 6, p%nqp, p%nelem))
       allocate(m%qp%Od(6, 6, p%nqp, p%nelem))
       allocate(m%qp%Pd(6, 6, p%nqp, p%nelem))
       allocate(m%qp%Qd(6, 6, p%nqp, p%nelem))
       allocate(m%qp%Sd(6, 6, p%nqp, p%nelem))
       allocate(m%qp%Xd(6, 6, p%nqp, p%nelem))
       allocate(m%qp%Yd(6, 6, p%nqp, p%nelem))

    case (1)
       allocate(m%qp%Gi(p%nqp, 6, 6, p%nelem))
       allocate(m%qp%Ki(p%nqp, 6, 6, p%nelem))
       allocate(m%qp%Mi(p%nqp, 6, 6, p%nelem))
       allocate(m%qp%Oe(p%nqp, 6, 6, p%nelem))
       allocate(m%qp%Pe(p%nqp, 6, 6, p%nelem))
       allocate(m%qp%Qe(p%nqp, 6, 6, p%nelem))
       allocate(m%qp%Gd(p%nqp, 6, 6, p%nelem))
       allocate(m%qp%Od(p%nqp, 6, 6, p%nelem))
       allocate(m%qp%Pd(p%nqp, 6, 6, p%nelem))
       allocate(m%qp%Qd(p%nqp, 6, 6, p%nelem))
       allocate(m%qp%Sd(p%nqp, 6, 6, p%nelem))
       allocate(m%qp%Xd(p%nqp, 6, 6, p%nelem))
       allocate(m%qp%Yd(p%nqp, 6, 6, p%nelem))

    case default
       write(*,*) "Invalid option provided. choose [0-1]"
       stop 1
    end select

    call random_number(m%qp%Gi)
    call random_number(m%qp%Ki)
    call random_number(m%qp%Mi)
    call random_number(m%qp%Oe)
    call random_number(m%qp%Pe)
    call random_number(m%qp%Qe)
    call random_number(m%qp%Gd)
    call random_number(m%qp%Od)
    call random_number(m%qp%Pd)
    call random_number(m%qp%Qd)
    call random_number(m%qp%Sd)
    call random_number(m%qp%Xd)
    call random_number(m%qp%Yd)
  end subroutine init_bdvars
end module bd_types
