!> @ingroup DerivedType
!> @{
!> @defgroup Data_Type_MeshDerivedType Data_Type_Mesh
!> Type_Mesh definition
!> @}

!> @ingroup Interface
!> @{
!> @defgroup Data_Type_MeshInterface Data_Type_Mesh
!> Type_Mesh definition
!> @}

!> @ingroup PublicProcedure
!> @{
!> @defgroup Data_Type_MeshPublicProcedure Data_Type_Mesh
!> Type_Mesh definition
!> @}

!> @ingroup PrivateProcedure
!> @{
!> @defgroup Data_Type_MeshPrivateProcedure Data_Type_Mesh
!> Type_Mesh definition
!> @}

!> This module contains the definition of Type_Mesh and its procedures.
module Data_Type_Mesh
!-----------------------------------------------------------------------------------------------------------------------------------
USE IR_Precision        ! Integers and reals precision definition.
USE Data_Type_Cell_Quad ! Definition of Type_Cell_Quad.
USE Data_Type_Node      ! Definition of Type_Node.
USE Data_Type_Face      ! Definition of Type_Face.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
private
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
!> Derived type containing conservative variables.
!> @ingroup Data_Type_MeshDerivedType
type, public:: Type_Mesh
  integer(I4P)::                      N = 0_I4P   !< Number of grid cells in each direction.
  real(R8P)::                         dh = 0._R8P !< Space step \f$dh=\frac{1}{N}\f$.
  real(R8P), allocatable::            x(:)        !< X Cartesian abscissa [0:N].
  real(R8P), allocatable::            y(:)        !< y Cartesian abscissa [0:N].

  integer(I4P)::                      Nnode = 0_I4P !< Number of nodes.
  integer(I4P)::                      Nface = 0_I4P !< Number of faces.
  integer(I4P)::                      Ncell = 0_I4P !< Number of cells.
  type(Type_Node),      allocatable:: node(:)       !< Nodes [1:Nnode].
  type(Type_Face),      allocatable:: face(:)       !< Nodes [1:Nface].
  type(Type_Cell_Quad), allocatable:: cell(:)       !< Cells [1:Ncell].
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
  class(Type_Mesh), intent(INOUT):: self   !< Mesh data.
  integer(I4P),     intent(IN)::    N      !< Number of grid cells.
  integer(I4P)::                    Nnode  !< Number of nodes.
  integer(I4P)::                    Nface  !< Number of faces.
  integer(I4P)::                    Ncell  !< Number of cells.
  integer(I4P)::                    ij     !< Counters.
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

  Nnode = (N+1)*(N+1)
  Nface = 2*N*(N+1)
  Ncell = N*N
  if (allocated(self%node)) deallocate(self%node) ; allocate(self%node(1:Nnode))
  if (allocated(self%face)) deallocate(self%face) ; allocate(self%face(1:Nface))
  if (allocated(self%cell)) deallocate(self%cell) ; allocate(self%cell(1:Ncell))
!  ij = 0
  !do j=0,N
    !do i=0,N
      !ij = ij + 1 ; call xyz%set(x=real(i,R8P)/real(N,R8P),y=real(j,R8P)/real(N,R8P),z=0._R8P) ; call self%node(ig)%initialize(xyz)
    !enddo
  !enddo
  !ij = 0
  !do j=1,N
    !do i=1,N
      !ij = ij + 1 ; call self%face(ij)%initialize(node=[self%node(ij)],N=null(),S=null())
    !enddo
!  enddo
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
