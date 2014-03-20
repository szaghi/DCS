!> @ingroup DerivedType
!> @{
!> @defgroup Data_Type_CavityDerivedType Data_Type_Cavity
!> Type_Cavity definition
!> @}

!> @ingroup Interface
!> @{
!> @defgroup Data_Type_CavityInterface Data_Type_Cavity
!> Type_Cavity definition
!> @}

!> @ingroup PublicProcedure
!> @{
!> @defgroup Data_Type_CavityPublicProcedure Data_Type_Cavity
!> Type_Cavity definition
!> @}

!> @ingroup PrivateProcedure
!> @{
!> @defgroup Data_Type_CavityPrivateProcedure Data_Type_Cavity
!> Type_Cavity definition
!> @}

!> This module contains the definition of Type_Cavity and its procedures.
module Data_Type_Cavity
!-----------------------------------------------------------------------------------------------------------------------------------
USE IR_Precision           ! Integers and reals precision definition.
USE Data_Type_Conservative ! Definition of Type_Conservative.
USE Data_Type_Mesh         ! Definition of Type_Mesh.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
private
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
!> Derived type containing conservative variables.
!> @ingroup Data_Type_CavityDerivedType
type, public:: Type_Cavity
  real(R8P)::                            Re   = 500._R8P !< Reynolds number.
  real(R8P)::                            beta = 0.6_R8P  !< Relaxation parameter.
  type(Type_Mesh)::                      mesh            !< Mesh data.
  type(Type_Conservative), allocatable:: cons(:,:)       !< Conservative variables [0,mesh%N,0:mesh%N].
  contains
    procedure, non_overridable:: initialize ! Procedure for initializing Type_Cavity.
    procedure, non_overridable:: finalize   ! Procedure for finalizing   Type_Cavity.
    procedure, non_overridable:: pprint     ! Procedure for printing Type_Cavity components with a "pretty" format.
endtype Type_Cavity
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  !> @ingroup Data_Type_CavityPublicProcedure
  !> @{
  !> @}

  !> @ingroup Data_Type_CavityPrivateProcedure
  !> @{
  !> @brief Subroutine for initializing Type_Cavity.
  elemental subroutine initialize(self,Re,beta,N)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Cavity), intent(INOUT):: self !< Cavity data.
  real(R8P),          intent(IN)::    Re   !< Reynolds number.
  real(R8P),          intent(IN)::    beta !< Relaxation parameter.
  integer(I4P),       intent(IN)::    N    !< Number of grid cells.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  self%Re = Re
  self%beta = beta
  call self%mesh%initialize(N=N)
  if (allocated(self%cons)) deallocate(self%cons) ; allocate(self%cons(0:N,0:N)) ! call self%cons%set(s=,v=)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine initialize

  !> @brief Subroutine for finalizing Type_Cavity.
  elemental subroutine finalize(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Cavity), intent(INOUT):: self !< Cavity data.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  self%Re = 1._R8P
  self%beta = 0.6_R8P
  call self%mesh%finalize
  if (allocated(self%cons)) deallocate(self%cons)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine finalize

  !> @brief Function for printing in a pretty ascii format the components of type Type_Cavity.
  !> @return \b err integer(I4P) variable for error trapping.
  function pprint(self,unit) result(err)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Cavity), intent(IN):: self !< Cavity variables.
  integer(I4P),       intent(IN):: unit !< Logic unit.
  integer(I4P)::                   err  !< Error trapping flag: 0 no errors, >0 error occurs.
  integer(I4P)::                   i,j  !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(unit,'(A)',iostat=err)' Re='//str(n=self%Re)
  write(unit,'(A)',iostat=err)' beta='//str(n=self%beta)
  write(unit,'(A)',iostat=err)' Mesh data'
  err = self%mesh%pprint(unit)
  write(unit,'(A)',iostat=err)' Conservative variables data'
  do j=0,self%mesh%N
    do i=0,self%mesh%N
      err = self%cons(i,j)%pprint(unit)
    enddo
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pprint
  !> @}
endmodule Data_Type_Cavity
