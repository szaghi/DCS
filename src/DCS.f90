! including modules definitions
#include "IR_Precision.f90"

!> @addtogroup Program Programs
!> List of excutable programs.
!> @addtogroup DerivedType Derived Types
!> List of derived data types.
!> @addtogroup GlobalVarPar Global Variables and Parameters
!> List of global variables and parameters.
!> @addtogroup PrivateVarPar Private Variables and Parameters
!> List of private variables and parameters.
!> @addtogroup Interface Interfaces
!> List of explicitly defined interface.
!> @addtogroup Library Modules Libraries
!> List of modules containing libraries of procedures.
!> @addtogroup PublicProcedure Public Procedures
!> List of public procedures.
!> @addtogroup PrivateProcedure Private Procedures
!> List of private procedures.

!> @ingroup DerivedType
!> @{
!> @defgroup Data_Type_ConservativeDerivedType Data_Type_Conservative
!> @}

!> @ingroup Interface
!> @{
!> @defgroup Data_Type_ConservativeInterface Data_Type_Conservative
!> @}

!> @ingroup PublicProcedure
!> @{
!> @defgroup Data_Type_ConservativePublicProcedure Data_Type_Conservative
!> @}

!> @ingroup PrivateProcedure
!> @{
!> @defgroup Data_Type_ConservativePrivateProcedure Data_Type_Conservative
!> @}

!> This module contains the definition of Type_Conservative and its procedures.
module Data_Type_Conservative
!-----------------------------------------------------------------------------------------------------------------------------------
USE IR_Precision ! Integers and reals precision definition.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
private
public:: computestream
public:: computevorticity
public:: computeresidual
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
!> Derived type containing conservative variables.
!> @ingroup Data_Type_ConservativeDerivedType
type, public:: Type_Conservative
  real(R8P):: s(1:2)=[0._R8P,0._R8P] !< Stream function:
                                     !< \f$[\frac{{\partial s}}{{\partial x}}=u,\frac{{\partial s}}{{\partial y}}=v]\f$,
                                     !< s(1) => s(n), s(2) => s(n+1).
  real(R8P):: v(1:2)=[0._R8P,0._R8P] !< Vorticity, v(1) => v(n), v(2) => v(n+1).
  contains
    procedure, non_overridable:: set       ! Procedure for setting variables.
    procedure, non_overridable:: oldupdate ! Procedure for updating old (n) variables to new (n+1) ones.
    procedure, non_overridable:: pprint    ! Procedure for printing Type_Conservative components with a "pretty" format.
endtype Type_Conservative
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  !> @ingroup Data_Type_ConservativePublicProcedure
  !> @{
  !> @brief Subroutine for computing new (n+1) stream function.
  pure subroutine computestream(cons,beta,dh)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  type(Type_Conservative), intent(INOUT):: cons(0:,0:) !< Conservative variables.
  real(R8P),               intent(IN)::    beta        !< Relaxation parameter.
  real(R8P),               intent(IN)::    dh          !< Space step.
  integer(I4P)::                           i,j         !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  do j=1_I4P,ubound(cons,dim=2)-1_I4P
    do i=1_I4P,ubound(cons,dim=1)-1_I4P
      cons(i,j)%s(2)=beta*((cons(i-1,j)%s(2)+cons(i+1,j)%s(2)+cons(i,j-1)%s(2)+cons(i,j+1)%s(2) + dh*dh*cons(i,j)%v(2))*0.25_R8P) +&
                     (1._R8P-beta)*cons(i,j)%s(2)
    enddo
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine computestream

  !> @brief Subroutine for computing new (n+1) vorticity.
  pure subroutine computevorticity(cons,Re,beta,dh)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  type(Type_Conservative), intent(INOUT):: cons(0:,0:) !< Conservative variables.
  real(R8P),               intent(IN)::    Re          !< Reynolds number.
  real(R8P),               intent(IN)::    beta        !< Relaxation parameter.
  real(R8P),               intent(IN)::    dh          !< Space step.
  integer(I4P)::                           N           !< Number of cells along each direction.
  integer(I4P)::                           ij,i,j      !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  N=ubound(cons,dim=1)
  do ij=1,N-1_I4P
    cons(ij,0)%v(2)=(                                                                                             &
                     -(cons(ij-1,1)%s(2) + cons(ij,1)%s(2) + cons(ij+1,1)%s(2))                   /(3._R8P*dh*dh) &
                     -(  0.5_R8P*cons(ij-1,0)%v(2) +                    0.5_R8P*cons(ij+1,0)%v(2)                 &
                       +0.25_R8P*cons(ij-1,1)%v(2) + cons(ij,1)%v(2) + 0.25_R8P*cons(ij+1,1)%v(2))/(9._R8P)       &
                    )*9._R8P*0.5_R8P
    cons(ij,N)%v(2)=(-1._R8P/dh                                                                                         &
                     -(cons(ij-1,N-1)%s(2)+cons(ij,N-1)%s(2) + cons(ij+1,N-1)%s(2))                     /(3._R8P*dh*dh) &
                     -(  0.5_R8P*cons(ij-1,N  )%v(2) +                      0.5_R8P*cons(ij+1,N  )%v(2)                 &
                       +0.25_R8P*cons(ij-1,N-1)%v(2) + cons(ij,N-1)%v(2) + 0.25_R8P*cons(ij+1,N-1)%v(2))/(9._R8P)       &
                    )*9._R8P*0.5_R8P
    cons(0,ij)%v(2)=(                                                                                             &
                     -(cons(1,ij-1)%s(2) + cons(1,ij)%s(2) + cons(1,ij+1)%s(2))                   /(3._R8P*dh*dh) &
                     -(  0.5_R8P*cons(0,ij-1)%v(2) +                    0.5_R8P*cons(0,ij+1)%v(2)                 &
                       +0.25_R8P*cons(1,ij-1)%v(2) + cons(1,ij)%v(2) + 0.25_R8P*cons(1,ij+1)%v(2))/(9._R8P)       &
                    )*9._R8P*0.5_R8P
    cons(N,ij)%v(2)=(                                                                                                   &
                     -(cons(N-1,ij-1)%s(2)+cons(N-1,ij)%s(2)+cons(N-1,ij+1)%s(2))                       /(3._R8P*dh*dh) &
                     -(  0.5_R8P*cons(N,ij-1  )%v(2) +                      0.5_R8P*cons(N,ij+1  )%v(2)                 &
                       +0.25_R8P*cons(N-1,ij-1)%v(2) + cons(N-1,ij)%v(2) + 0.25_R8P*cons(N-1,ij+1)%v(2))/(9._R8P)       &
                    )*9._R8P*0.5_R8P
  enddo
  cons(0,0)%v(2)=(                                                                                  &
                  -(cons(1,1)%s(2))/(3._R8P*dh*dh)                                                  &
                  -(0.5_R8P*cons(1,0)%v(2)+0.5_R8P*cons(0,1)%v(2)+0.25_R8P*cons(1,1)%v(2))/(9._R8P) &
                 )*9._R8P
  cons(N,0)%v(2)=(                                                                                      &
                  -(cons(N-1,1)%s(2))/(3._R8P*dh*dh)                                                    &
                  -(0.5_R8P*cons(N-1,0)%v(2)+0.5_R8P*cons(N,1)%v(2)+0.25_R8P*cons(N-1,1)%v(2))/(9._R8P) &
                 )*9._R8P
  cons(N,N)%v(2)=(-0.5_R8P/dh                                                                               &
                  -(cons(N-1,N-1)%s(2))/(3._R8P*dh*dh)                                                      &
                  -(0.5_R8P*cons(N-1,N)%v(2)+0.5_R8P*cons(N,N-1)%v(2)+0.25_R8P*cons(N-1,N-1)%v(2))/(9._R8P) &
                 )*9._R8P
  cons(0,N)%v(2)=(-0.5_R8P/dh                                                                           &
                  -(cons(1,N-1)%s(2))/(3._R8P*dh*dh)                                                    &
                  -(0.5_R8P*cons(1,N)%v(2)+0.5_R8P*cons(0,N-1)%v(2)+0.25_R8P*cons(1,N-1)%v(2))/(9._R8P) &
                 )*9._R8P
  do j=1,N-1
    do i=1,N-1
       cons(i,j)%v(2)=beta*((                                                                                   &
                             cons(i-1,j)%v(2)+cons(i+1,j)%v(2)+cons(i,j-1)%v(2)+cons(i,j+1)%v(2)                &
                             -0.25_R8P*Re*(cons(i,j+1)%s(2)-cons(i,j-1)%s(2))*(cons(i+1,j)%v(2)-cons(i-1,j)%v(2)) &
                             +0.25_R8P*Re*(cons(i+1,j)%s(2)-cons(i-1,j)%s(2))*(cons(i,j+1)%v(2)-cons(i,j-1)%v(2)) &
                            )*0.25_R8P) + (1._R8P-beta)*cons(i,j)%v(2)
    enddo
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine computevorticity

  !> @brief Function for computing residuals.
  pure function computeresidual(cons,Re,dh) result(residual)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  type(Type_Conservative), intent(IN):: cons(0:,0:) !< Conservative variables.
  real(R8P),               intent(IN):: Re          !< Reynolds number.
  real(R8P),               intent(IN):: dh          !< Space step.
  type(Type_Conservative)::             residual    !< Conservative variables residuals.
  integer(I4P)::                        i,j         !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  residual%s(2)=0._R8P
  residual%v(2)=0._R8P
  do j=1_I4P,ubound(cons,dim=2)-1_I4P
    do i=1_I4P,ubound(cons,dim=1)-1_I4P
      residual%s(2) =max(residual%s(2),                                                          &
                         abs((cons(i-1,j)%s(2)-2._R8P*cons(i,j)%s(2)+cons(i+1,j)%s(2))/(dh*dh) + &
                             (cons(i,j-1)%s(2)-2._R8P*cons(i,j)%s(2)+cons(i,j+1)%s(2))/(dh*dh) + cons(i,j)%v(2)))
      residual%v(2) =max(residual%v(2),                                                                                        &
                         abs((1._R8P/Re)*(cons(i-1,j)%v(2)-2._R8P*cons(i,j)%v(2)+cons(i+1,j)%v(2))/(dh*dh)                   + &
                             (1._R8P/Re)*(cons(i,j-1)%v(2)-2._R8P*cons(i,j)%v(2)+cons(i,j+1)%v(2))/(dh*dh)                   - &
                             (cons(i,j+1)%s(2)-cons(i,j-1)%s(2))/(2._R8P*dh)*(cons(i+1,j)%v(2)-cons(i-1,j)%v(2))/(2._R8P*dh) + &
                             (cons(i+1,j)%s(2)-cons(i-1,j)%s(2))/(2._R8P*dh)*(cons(i,j+1)%v(2)-cons(i,j-1)%v(2))/(2._R8P*dh)))
    enddo
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction computeresidual
  !> @}

  !> @ingroup Data_Type_ConservativePrivateProcedure
  !> @{
  !> @brief Subroutine for
  elemental subroutine set(self,s,v)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Conservative), intent(INOUT):: self !< Conservative variables.
  real(R8P),                intent(IN)::    s    !< Stream function.
  real(R8P),                intent(IN)::    v    !< Vorticity.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  self%s = s
  self%v = v
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine set

  !> @brief Subroutine for updating old (n) variables to new (n+1) ones.
  elemental subroutine oldupdate(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Conservative), intent(INOUT):: self !< Conservative variables.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  self%s(1) = self%s(2)
  self%v(1) = self%v(2)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine oldupdate

  !> @brief Function for printing in a pretty ascii format the components of type Type_Conservative.
  !> @return \b err integer(I4P) variable for error trapping.
  function pprint(self,unit) result(err)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Conservative), intent(IN):: self !< Conservative variables.
  integer(I4P),             intent(IN):: unit !< Logic unit.
  integer(I4P)::                         err  !< Error trapping flag: 0 no errors, >0 error occurs.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(unit,'(A)',iostat=err)' s(n)='//str(n=self%s(1))//' s(n+1)='//str(n=self%s(2))
  write(unit,'(A)',iostat=err)' v(n)='//str(n=self%v(1))//' v(n+1)='//str(n=self%v(2))
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pprint
  !> @}
endmodule Data_Type_Conservative

!> @ingroup DerivedType
!> @{
!> @defgroup Data_Type_MeshDerivedType Data_Type_Mesh
!> @}

!> @ingroup Interface
!> @{
!> @defgroup Data_Type_MeshInterface Data_Type_Mesh
!> @}

!> @ingroup PublicProcedure
!> @{
!> @defgroup Data_Type_MeshPublicProcedure Data_Type_Mesh
!> @}

!> @ingroup PrivateProcedure
!> @{
!> @defgroup Data_Type_MeshPrivateProcedure Data_Type_Mesh
!> @}

!> This module contains the definition of Type_Mesh and its procedures.
module Data_Type_Mesh
!-----------------------------------------------------------------------------------------------------------------------------------
USE IR_Precision ! Integers and reals precision definition.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
private
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
!> Derived type containing conservative variables.
!> @ingroup Data_Type_MeshDerivedType
type, public:: Type_Mesh
  integer(I4P)::           N = 0_I4P   !< Number of grid cells in each direction.
  real(R8P)::              dh = 0._R8P !< Space step \f$dh=\frac{1}{N}\f$.
  real(R8P), allocatable:: x(:)        !< X Cartesian abscissa [0:N].
  real(R8P), allocatable:: y(:)        !< y Cartesian abscissa [0:N].
  contains
    procedure, non_overridable:: initialize ! Procedure for initializing Type_Mesh.
    procedure, non_overridable:: finalize   ! Procedure for finalizing   Type_Mesh.
    procedure, non_overridable:: pprint     ! Procedure for printing Type_Mesh components with a "pretty" format.
endtype Type_Mesh
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  !> @ingroup Data_Type_MeshPublicProcedure
  !> @{
  !> @}

  !> @ingroup Data_Type_MeshPrivateProcedure
  !> @{
  !> @brief Subroutine for initializing Type_Mesh.
  elemental subroutine initialize(self,N)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Mesh), intent(INOUT):: self !< Mesh data.
  integer(I4P),     intent(IN)::    N    !< Number of grid cells.
  integer(I4P)::                    ij   !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  self%N = N
  self%dh = 1._R8P/real(N,R8P)
  if (allocated(self%x)) deallocate(self%x) ; allocate(self%x(0:N))
  if (allocated(self%y)) deallocate(self%y) ; allocate(self%y(0:N))
  do ij=0,N
    self%x(ij) = real(ij,R8P)/real(N,R8P)
  enddo
  self%y = self%x
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine initialize

  !> @brief Subroutine for finalizing Type_Mesh.
  elemental subroutine finalize(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Mesh), intent(INOUT):: self !< Mesh data.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  self%N = 0_I4P
  self%dh = 0._R8P
  if (allocated(self%x)) deallocate(self%x)
  if (allocated(self%y)) deallocate(self%y)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine finalize

  !> @brief Function for printing in a pretty ascii format the components of type Type_Mesh.
  !> @return \b err integer(I4P) variable for error trapping.
  function pprint(self,unit) result(err)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Mesh), intent(IN):: self !< Mesh data.
  integer(I4P),     intent(IN):: unit !< Logic unit.
  integer(I4P)::                 err  !< Error trapping flag: 0 no errors, >0 error occurs.
  integer(I4P)::                 ij   !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(unit,'(A)',iostat=err)' N='//str(n=self%N)
  if (allocated(self%x).AND.allocated(self%y)) then
    do ij=lbound(self%x,dim=1),ubound(self%x,dim=1)
      write(unit,'(A)',iostat=err)' ij='//str(n=ij)//' x='//str(n=self%x(ij))//' y='//str(n=self%y(ij))
    enddo
  endif
  write(unit,'(A)',iostat=err)' dh='//str(n=self%dh)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pprint
  !> @}
endmodule Data_Type_Mesh

!> @ingroup DerivedType
!> @{
!> @defgroup Data_Type_CavityDerivedType Data_Type_Cavity
!> @}

!> @ingroup Interface
!> @{
!> @defgroup Data_Type_CavityInterface Data_Type_Cavity
!> @}

!> @ingroup PublicProcedure
!> @{
!> @defgroup Data_Type_CavityPublicProcedure Data_Type_Cavity
!> @}

!> @ingroup PrivateProcedure
!> @{
!> @defgroup Data_Type_CavityPrivateProcedure Data_Type_Cavity
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
  real(R8P)::                            Re   = 1._R8P  !< Reynolds number.
  real(R8P)::                            beta = 0.6_R8P !< Relaxation parameter.
  type(Type_Mesh)::                      mesh           !< Mesh data.
  type(Type_Conservative), allocatable:: cons(:,:)      !< Conservative variables [0,mesh%N,0:mesh%N].
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

!> @ingroup Program
!> @{
!> @defgroup DCSProgram DCS
!> @}

!> @ingroup PrivateVarPar
!> @{
!> @defgroup DCSPrivateVarPar DCS
!> @}

!> @ingroup PrivateProcedure
!> @{
!> @defgroup DCSPrivateProcedure DCS
!> @}

!> @brief DCS is an Open source program for simulating driven cavity problems.
!> It is written in in standard (compliant) Fortran 2003.
!> @author    Stefano Zaghi
!> @author    Emilio F. Campana
!> @version   0.0.1
!> @date      2013-07-01
!> @copyright GNU Public License version 3.
!> @todo \b DocImprove: Improve the documentation
!> @ingroup DCSProgram
program DCS
!-----------------------------------------------------------------------------------------------------------------------------------
USE, intrinsic:: ISO_FORTRAN_ENV, only: stdout => OUTPUT_UNIT, stderr => ERROR_UNIT ! Standard output/error logical units.
USE IR_Precision                                                                    ! Integers and reals precision definition.
USE Data_Type_Cavity                                                                ! Definition of Type_Cavity.
USE Data_Type_Conservative                                                          ! Definition of Type_Conservative.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
!> @ingroup DCSPrivateVarPar
!> @{
type(Type_Cavity):: cavity       !< Cavity data.
real(R8P)::         rtol = 1.E-6 !< Residual tolerance.
integer(I4P)::      nout = 1_I4P !< Console updating frequency.
!> @}
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
call dcs_initialize ! initializing the simulation
call dcs_simulate   ! performing the simulation
call dcs_finalize   ! finalizing the simulation
stop
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  !> @ingroup DCSPrivateProcedure
  !> @{
  !> @brief The Get_Unit function returns a free logic unit for opening a file. The unit value is returned by the function, and also
  !> by the optional argument "Free_Unit". This allows the function to be used directly in an open statement like:
  !> open(unit=Get_Unit(myunit),...) ; read(myunit)...
  !> If no units are available, -1 is returned.
  integer function Get_Unit(Free_Unit)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer, intent(OUT), optional:: Free_Unit !< Free logic unit.
  integer::                        n1        !< Counter.
  integer::                        ios       !< Inquiring flag.
  logical::                        lopen     !< Inquiring flag.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  Get_Unit = -1
  n1=1
  do
    if ((n1/=stdout).AND.(n1/=stderr)) then
      inquire (unit=n1,opened=lopen,iostat=ios)
      if (ios==0) then
        if (.NOT.lopen) then
          Get_Unit = n1 ; if (present(Free_Unit)) Free_Unit = Get_Unit
          return
        endif
      endif
    endif
    n1=n1+1
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction Get_Unit

  !> @brief Subroutine for printing to stdout the correct usage of the code.
  subroutine print_usage()
  !---------------------------------------------------------------------------------------------------------------------------------
  ! Subroutine for printing the correct use of the program.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(stdout,'(A)')' DCS'
  write(stdout,'(A)')' Driven-Cavity Simulator code'
  write(stdout,'(A)')' Usage:'
  write(stdout,*)
  write(stdout,'(A)')' Examples: '
  write(stdout,*)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine print_usage

  !> @brief Subroutine for initializing the simulation according to the input options.
  subroutine dcs_initialize()
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  real(R8P)::      Re        !< Reynolds number.
  real(R8P)::      beta      !< Relaxation parameter.
  integer(I4P)::   N         !< Number of grid cells in each direction.
  integer(I4P)::   Nca = 0   !< Number of command line arguments.
  integer(I4P)::   c         !< Counter for command line arguments.
  character(5)::   ca_switch !< Switch identifier.
  character(100):: sbuf      !< String buffer.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(stdout,'(A)')' Some information about the precision of runnig machine'
  call IR_Print()
  write(stdout,*)
  ! parsing command line arguments
  Nca = command_argument_count()
  if (Nca==0) then
    write(stderr,'(A)')' Error: no argument has been passed to command line'
    call print_usage
    stop
  else
    ! processing switch
    c = 0
    do while (c<Nca)
      c = c + 1
      call get_command_argument(c, ca_switch)
      select case(adjustl(trim(ca_switch)))
      case('-Re')
        call get_command_argument(c+1,sbuf) ; c = c + 1
        read(sbuf,*)Re
      case('-beta')
        call get_command_argument(c+1,sbuf) ; c = c + 1
        read(sbuf,*)beta
      case('-N')
        call get_command_argument(c+1,sbuf) ; c = c + 1
        read(sbuf,*)N
      case('-rtol')
        call get_command_argument(c+1,sbuf) ; c = c + 1
        read(sbuf,*)rtol
      case('-nout')
        call get_command_argument(c+1,sbuf) ; c = c + 1
        read(sbuf,*)nout
      case default
        write(stderr,'(A)') ' Error: switch "'//adjustl(trim(ca_switch))//'" is unknown'
        call print_usage
        stop
      endselect
    enddo
  endif
  write(stdout,'(A)')' Simulation parameters'
  write(stdout,'(A)')' Re='//trim(str(n=Re))
  write(stdout,'(A)')' beta='//trim(str(n=beta))
  write(stdout,'(A)')' N='//trim(str(n=N))
  write(stdout,'(A)')' rtol='//trim(str(n=rtol))
  write(stdout,'(A)')' nout='//trim(str(n=nout))
  write(stdout,*)
  ! initializing cavity data
  call cavity%initialize(Re=Re,beta=beta,N=N)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine dcs_initialize

  !> @brief Subroutine for simulating drive cavity problems.
  subroutine dcs_simulate()
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  type(Type_Conservative):: residual  !< Residual of current (n) iteration.
  integer(I4P)::            n = 0_I4P !< Time steps counter.
  integer(I4P)::            i,j       !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  ! successive over-relaxation (SOR) loop
  call residual%set(s=MaxR8P,v=MaxR8P)
  SOR: do while(((residual%s(2)>rtol).AND.(residual%v(2)>rtol)))
    ! updating time step counter
    n = n + 1_I4P
    ! updating old conservative variables: cons(n) = cons(n+1)
    do j=1,cavity%mesh%N-1_I4P
      do i=1,cavity%mesh%N-1_I4P
        call cavity%cons(i,j)%oldupdate
      enddo
    enddo
    ! computing new stream function
    call computestream(cons=cavity%cons,beta=cavity%beta,dh=cavity%mesh%dh)
    ! computing new vorticity
    call computevorticity(cons=cavity%cons,Re=cavity%Re,beta=cavity%beta,dh=cavity%mesh%dh)
    ! computing residuals
    residual = computeresidual(cons=cavity%cons,Re=cavity%Re,dh=cavity%mesh%dh)
    if (mod(n,nout)==0) then
      write(stdout,'(A)')' iteration n='//trim(str(n=n))//&
                         ' residuals='//trim(str(n=residual%s(2)))//','//trim(str(n=residual%v(2)))
    endif
  enddo SOR
  write(stdout,'(A)')' Convergence achieved n='//trim(str(n=n))
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine dcs_simulate

  !> @brief Subroutine for finalizing the simulation.
  subroutine dcs_finalize()
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P):: u   !< Logical unit for output file.
  integer(I4P):: i,j !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  ! writing solution
  open(unit=Get_Unit(u),file='DCS_out.dat')
  write(u,'(A)')'VARIABLES="x" "y" "s" "v"'
  write(u,'(A)')'ZONE T="Driven Cavity" I='//trim(str(n=cavity%mesh%N+1))//' J='//trim(str(n=cavity%mesh%N+1))
  do j=0,cavity%mesh%N
    do i=0,cavity%mesh%N
      write(u,'(A)')trim(str(n=cavity%mesh%x(i)))//' '//trim(str(n=cavity%mesh%y(j)))//' '//&
                    trim(str(n=cavity%cons(i,j)%s(2)))//' '//trim(str(n=cavity%cons(i,j)%v(2)))
    enddo
  enddo
  close(u)
  ! finalizing cavity data
  call cavity%finalize
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine dcs_finalize
  !> @}
endprogram DCS
