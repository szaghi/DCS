!> @ingroup DerivedType
!> @{
!> @defgroup Data_Type_FaceDerivedType Data_Type_Face
!> Type_Face definition
!> @}

!> @ingroup Interface
!> @{
!> @defgroup Data_Type_FaceInterface Data_Type_Face
!> Type_Face definition
!> @}

!> @ingroup PublicProcedure
!> @{
!> @defgroup Data_Type_FacePublicProcedure Data_Type_Face
!> Type_Face definition
!> @}

!> @ingroup PrivateProcedure
!> @{
!> @defgroup Data_Type_FacePrivateProcedure Data_Type_Face
!> Type_Face definition
!> @}

!> This module contains the definition of Type_Face and its procedures.
module Data_Type_Face
!-----------------------------------------------------------------------------------------------------------------------------------
USE IR_Precision     ! Integers and reals precision definition.
USE Data_Type_Node   ! Definition of Type_Node.
USE Data_Type_Vector ! Definition of Type_Vector.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
private
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
!> Derived type containing conservative variables.
!> @ingroup Data_Type_FaceDerivedType
type, public:: Type_Face
  type(Type_Node_Ptr)::        node(1:2)   !< Nodes data.
  type(Type_Vector), pointer:: N           !< Face outward unit normals.
  real(R8P),         pointer:: S => null() !< Face area.
  contains
    procedure, non_overridable:: initialize ! Procedure for initializing Type_Face.
    procedure, non_overridable:: finalize   ! Procedure for finalizing   Type_Face.
    procedure, non_overridable:: pprint     ! Procedure for printing Type_Face components with a "pretty" format.
endtype Type_Face
!> Pointer of Type_Face for creating array of pointers of Type_Face.
!> @ingroup Data_Type_FaceDerivedType
type, public:: Type_Face_Ptr
  type(Type_Face), pointer:: p => null()
endtype Type_Face_Ptr
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  !> @ingroup Data_Type_FacePublicProcedure
  !> @{
  !> @}

  !> @ingroup Data_Type_FacePrivateProcedure
  !> @{
  !> @brief Subroutine for initializing Type_Face.
  subroutine initialize(self,node1,node2,N,S)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Face),           intent(INOUT):: self  !< data.
  type(Type_Node),   pointer, intent(IN)::    node1 !< Node 1 data.
  type(Type_Node),   pointer, intent(IN)::    node2 !< Node 2 data.
  type(Type_Vector), pointer, intent(IN)::    N     !< Face outward unit normals.
  real(R8P),         pointer, intent(IN)::    S     !< Face area.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  self%node(1)%p => node1
  self%node(2)%p => node2
  self%N         => N
  self%S         => S
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine initialize

  !> @brief Subroutine for finalizing Type_Face.
  subroutine finalize(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Face), intent(INOUT):: self !< data.
  integer(I4P)::                    n    !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  do n=lbound(self%node,dim=1),ubound(self%node,dim=1)
    if (associated(self%node(n)%p)) deallocate(self%node(n)%p) ; self%node(n)%p => null()
  enddo
  if (associated(self%N)) deallocate(self%N) ; self%N => null()
  if (associated(self%S)) deallocate(self%S) ; self%S => null()
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine finalize

  !> @brief Function for printing in a pretty ascii format the components of type Type_Face.
  !> @return \b err integer(I4P) variable for error trapping.
  function pprint(self,unit) result(err)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Face), intent(IN):: self !< data.
  integer(I4P),     intent(IN):: unit !< Logic unit.
  integer(I4P)::                 err  !< Error trapping flag: 0 no errors, >0 error occurs.
  integer(I4P)::                 n    !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(unit,'(A)',iostat=err)' Nodes data:'
  do n=lbound(self%node,dim=1),ubound(self%node,dim=1)
    if (associated(self%node(n)%p)) then
      write(unit,'(A)',iostat=err)' n='//trim(str(n=n))
      err = self%node(n)%p%pprint(unit=unit)
    endif
  enddo
  if (associated(self%N)) then
    write(unit,'(A)',iostat=err)' Face outward unit normal:'
    write(unit,'(A)',iostat=err)' x='//trim(str(n=self%N%x))//' y='//trim(str(n=self%N%y))//' z='//trim(str(n=self%N%z))
  endif
  write(unit,'(A)',iostat=err)' Face area='//trim(str(n=self%S))
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pprint
  !> @}
endmodule Data_Type_Face
