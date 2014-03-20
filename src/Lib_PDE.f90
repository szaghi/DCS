!> @ingroup Library
!> @{
!> @defgroup Lib_PDELibrary Lib_PDE
!> Partial Differential Equations solving library
!> @}

!> @ingroup Interface
!> @{
!> @defgroup Lib_PDEInterface Lib_PDE
!> Partial Differential Equations solving library
!> @}

!> @ingroup PublicProcedure
!> @{
!> @defgroup Lib_PDEPublicProcedure Lib_PDE
!> Partial Differential Equations solving library
!> @}

!> @ingroup PrivateProcedure
!> @{
!> @defgroup Lib_PDEPrivateProcedure Lib_PDE
!> Partial Differential Equations solving library
!> @}

!> @ingroup GlobalVarPar
!> @{
!> @defgroup Lib_PDEGlobalVarPar Lib_PDE
!> Partial Differential Equations solving library
!> @}

!> @ingroup PrivateVarPar
!> @{
!> @defgroup Lib_PDEPrivateVarPar Lib_PDE
!> Partial Differential Equations solving library
!> @}

!> @brief This module contains Partial Differential Equations (PDE) solving procedures.
!> @ingroup Lib_PDELibrary
module Lib_PDE
!-----------------------------------------------------------------------------------------------------------------------------------
USE IR_Precision ! Integers and reals precision definition.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public::
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
!> @ingroup Lib_PDEGlobalVarPar
!> @{
!> @}
!> @ingroup Lib_PDEPrivateVarPar
!> @{
!> @}
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
!> @brief Function for computing first partial derivative by means of Finite Volumes approach (Green's theorem).
!> Using the Green's theorem the first partial derivative of \f$u\f$ in \f$\vec i\f$ direction could be written as
!> \f$\frac{{\partial u}}{{\partial i}} =\frac{1}{V}\sum\limits_{f = 1}^6 {{u_f}\overrightarrow {{n_f}} \cdot \vec i\,{S_f}}\f$
!> being \f$V\f$ the value of finite volume, \f$\overrightarrow {{n_f}}\f$ the outward unit normal of \f$f^{th}\f$ face which
!> area is \f$S_f\f$.
!> @note It is assumed that the finite volume is discretized by means of a hexahedron.
!> @return \b fpd real variable.
!> @ingroup Lib_PDEInterface
interface dudi_FV
  module procedure dudi_FV_R8
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  !> @ingroup Lib_PDEPrivateProcedure
  !> @{
  !> Function for computing first partial derivative by means of Finite Volumes approach (Green's theorem).
  !> Using the Green's theorem the first partial derivative of \f$u\f$ in \f$\vec i\f$ direction could be written as
  !> \f$\frac{{\partial u}}{{\partial i}} =\frac{1}{V}\sum\limits_{f = 1}^6 {{u_f}\overrightarrow {{n_f}} \cdot \vec i\,{S_f}}\f$
  !> being \f$V\f$ the value of finite volume, \f$\overrightarrow {{n_f}}\f$ the outward unit normal of \f$f^{th}\f$ face which
  !> area is \f$S_f\f$.
  !> @note It is assumed that the finite volume is discretized by means of a hexahedron.
  !> @return \b fpd real(R8P) variable.
  pure function dudi_FV_R8(u,nsi,v) result(fpd)
  !-------------------------------------------------------------------------------------------------------------------------------
  implicit none
  real(R8P), intent(IN):: u(1:6)   !< Values of variable to be differentiated at each of 6 interfaces surrounding finite volume.
  real(R8P), intent(IN):: nsi(1:6) !< Area of 6 interfaces surrounding finite volume multiplied by normals projected along 'i'.
  real(R8P), intent(IN):: v        !< Value of finite volume.
  real(R8P)::             fpd      !< First partial derivative of 'u' in 'i' direction.
  !-------------------------------------------------------------------------------------------------------------------------------

  !-------------------------------------------------------------------------------------------------------------------------------
  fpd = (u(2)*nsi(2) - u(1)*nsi(1) + u(4)*nsi(4) - u(3)*nsi(3) + u(6)*nsi(6) - u(5)*nsi(5))/v
  return
  !-------------------------------------------------------------------------------------------------------------------------------
  endfunction dudi_FV_R8
  !> @}
endmodule Lib_PDE
