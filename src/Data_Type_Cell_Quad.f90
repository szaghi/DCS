!> @ingroup DerivedType
!> @{
!> @defgroup Data_Type_Cell_QuadDerivedType Data_Type_Cell_Quad
!> Type_Cell_Quad definition
!> @}

!> @ingroup Interface
!> @{
!> @defgroup Data_Type_Cell_QuadInterface Data_Type_Cell_Quad
!> Type_Cell_Quad definition
!> @}

!> @ingroup PublicProcedure
!> @{
!> @defgroup Data_Type_Cell_QuadPublicProcedure Data_Type_Cell_Quad
!> Type_Cell_Quad definition
!> @}

!> @ingroup PrivateProcedure
!> @{
!> @defgroup Data_Type_Cell_QuadPrivateProcedure Data_Type_Cell_Quad
!> Type_Cell_Quad definition
!> @}

!> This module contains the definition of Type_Cell_Quad and its procedures.
module Data_Type_Cell_Quad
!-----------------------------------------------------------------------------------------------------------------------------------
USE IR_Precision   ! Integers and reals precision definition.
USE Data_Type_Face ! Definition of Type_Face.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
private
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
!> Derived type containing conservative variables.
!> @ingroup Data_Type_Cell_QuadDerivedType
type, public:: Type_Cell_Quad
  type(Type_Face_Ptr):: face(1:4)    !< Faces data.
  real(R8P)::           vol = 0._R8P !< Volume.
  contains
    procedure, non_overridable:: initialize ! Procedure for initializing Type_Cell_Quad.
    procedure, non_overridable:: finalize   ! Procedure for finalizing   Type_Cell_Quad.
    procedure, non_overridable:: pprint     ! Procedure for printing Type_Cell_Quad components with a "pretty" format.
endtype Type_Cell_Quad
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  !> @ingroup Data_Type_Cell_QuadPublicProcedure
  !> @{
  !> @}

  !> @ingroup Data_Type_Cell_QuadPrivateProcedure
  !> @{
  !> @brief Subroutine for initializing Type_Cell_Quad.
  subroutine initialize(self,face1,face2,face3,face4,vol)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Cell_Quad),    intent(INOUT):: self  !< Cell data.
  type(Type_Face), pointer, intent(IN)::    face1 !< Face 1 data.
  type(Type_Face), pointer, intent(IN)::    face2 !< Face 2 data.
  type(Type_Face), pointer, intent(IN)::    face3 !< Face 3 data.
  type(Type_Face), pointer, intent(IN)::    face4 !< Face 4 data.
  real(R8P),                intent(IN)::    vol   !< Volume.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  self%face(1)%p => face1
  self%face(2)%p => face2
  self%face(3)%p => face3
  self%face(4)%p => face4
  self%vol       =  vol
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine initialize

  !> @brief Subroutine for finalizing Type_Cell_Quad.
  subroutine finalize(self)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Cell_Quad), intent(INOUT):: self !< Cell data.
  integer(I4P)::                         n    !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  do n=lbound(self%face,dim=1),ubound(self%face,dim=1)
    call self%face(n)%p%finalize
  enddo
  self%vol = 0._R8P
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine finalize

  !> @brief Function for printing in a pretty ascii format the components of type Type_Cell_Quad.
  !> @return \b err integer(I4P) variable for error trapping.
  function pprint(self,unit) result(err)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Cell_Quad), intent(IN):: self !< Cell data.
  integer(I4P),          intent(IN):: unit !< Logic unit.
  integer(I4P)::                      err  !< Error trapping flag: 0 no errors, >0 error occurs.
  integer(I4P)::                      n    !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  n = 0 ; err = n
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pprint
  !> @}
endmodule Data_Type_Cell_Quad
