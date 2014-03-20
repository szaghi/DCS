!> @ingroup DerivedType
!> @{
!> @defgroup Data_Type_NodeDerivedType Data_Type_Node
!> Type_Node definition
!> @}

!> @ingroup Interface
!> @{
!> @defgroup Data_Type_NodeInterface Data_Type_Node
!> Type_Node definition
!> @}

!> @ingroup PublicProcedure
!> @{
!> @defgroup Data_Type_NodePublicProcedure Data_Type_Node
!> Type_Node definition
!> @}

!> @ingroup PrivateProcedure
!> @{
!> @defgroup Data_Type_NodePrivateProcedure Data_Type_Node
!> Type_Node definition
!> @}

!> This module contains the definition of Type_Node and its procedures.
module Data_Type_Node
!-----------------------------------------------------------------------------------------------------------------------------------
USE IR_Precision     ! Integers and reals precision definition.
USE Data_Type_Vector ! Definition of Type_Vector.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
private
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
!> Derived type containing conservative variables.
!> @ingroup Data_Type_NodeDerivedType
type, public:: Type_Node
  type(Type_Vector):: xyz !< Node Cartesian coordinates.
  contains
    procedure, non_overridable:: initialize ! Procedure for initializing Type_Node.
    procedure, non_overridable:: finalize   ! Procedure for finalizing   Type_Node.
    procedure, non_overridable:: pprint     ! Procedure for printing Type_Node components with a "pretty" format.
endtype Type_Node
!> Pointer of Type_Node for creating array of pointers of Type_Node.
!> @ingroup Data_Type_NodeDerivedType
type, public:: Type_Node_Ptr
  type(Type_Node), pointer:: p => null()
endtype Type_Node_Ptr
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  !> @ingroup Data_Type_NodePublicProcedure
  !> @{
  !> @}

  !> @ingroup Data_Type_NodePrivateProcedure
  !> @{
  !> @brief Subroutine for initializing Type_Node.
  elemental subroutine initialize(self,xyz)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Node),  intent(INOUT):: self !< Node data.
  type(Type_Vector), intent(IN)::    xyz  !< Node Cartesian coordinates.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  self%xyz = xyz
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine initialize

  !> @brief Subroutine for finalizing Type_Node.
  elemental subroutine finalize(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Node), intent(INOUT):: self !< Node data.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine finalize

  !> @brief Function for printing in a pretty ascii format the components of type Type_Node.
  !> @return \b err integer(I4P) variable for error trapping.
  function pprint(self,unit) result(err)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Node), intent(IN):: self !< Node data.
  integer(I4P),     intent(IN):: unit !< Logic unit.
  integer(I4P)::                 err  !< Error trapping flag: 0 no errors, >0 error occurs.
  integer(I4P)::                 n    !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(unit,'(A)',iostat=err)' x='//trim(str(n=self%xyz%x))//' y='//trim(str(n=self%xyz%y))//' z='//trim(str(n=self%xyz%z))
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pprint
  !> @}
endmodule Data_Type_Node
