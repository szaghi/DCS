!> @ingroup DerivedType
!> @{
!> @defgroup Data_Type_ConservativeDerivedType Data_Type_Conservative
!> Type_Conservative definition
!> @}

!> @ingroup Interface
!> @{
!> @defgroup Data_Type_ConservativeInterface Data_Type_Conservative
!> Type_Conservative definition
!> @}

!> @ingroup PublicProcedure
!> @{
!> @defgroup Data_Type_ConservativePublicProcedure Data_Type_Conservative
!> Type_Conservative definition
!> @}

!> @ingroup PrivateProcedure
!> @{
!> @defgroup Data_Type_ConservativePrivateProcedure Data_Type_Conservative
!> Type_Conservative definition
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
  real(R8P):: s=0._R8P !< Stream function \f$[\frac{{\partial s}}{{\partial x}}=u,\frac{{\partial s}}{{\partial y}}=v]\f$.
  real(R8P):: v=0._R8P !< Vorticity.
  contains
    procedure, non_overridable:: set    ! Procedure for setting variables.
    procedure, non_overridable:: pprint ! Procedure for printing Type_Conservative components with a "pretty" format.
endtype Type_Conservative
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  !> @ingroup Data_Type_ConservativePublicProcedure
  !> @{
  !> @brief Subroutine for computing new (n+1) stream function.
  !> The second order derivatives of the Laplacian operator of the stream function equation are approximated by means of second
  !> order central differences and then the resulting finite difference equation is solved by means of a Successive Over
  !> Relaxation (SOR) approach:
  !> \f$s_{i,j}=\beta\frac{s_{i+1,j}+s_{i-1,j}+s_{i,j+1}+s_{i,j-1}+\Delta h^2v_{i,j}}{4}+\left(1-\beta\right)s_{i,j}\f$
  pure subroutine computestream(cons,beta,dh)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  type(Type_Conservative), intent(INOUT):: cons(0:,0:) !< Conservative variables.
  real(R8P),               intent(IN)::    beta        !< Relaxation parameter.
  real(R8P),               intent(IN)::    dh          !< Space step.
  integer(I4P)::                           i,j         !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  !$OMP PARALLEL DEFAULT(NONE) &
  !$OMP PRIVATE(i,j)           &
  !$OMP SHARED(cons,beta,dh)
  !$OMP DO COLLAPSE(2)
  do j=1_I4P,ubound(cons,dim=2)-1_I4P
    do i=1_I4P,ubound(cons,dim=1)-1_I4P
      cons(i,j)%s=beta*((cons(i-1,j)%s+cons(i+1,j)%s+cons(i,j-1)%s+cons(i,j+1)%s + dh*dh*cons(i,j)%v)*0.25_R8P) +&
                  (1._R8P-beta)*cons(i,j)%s
    enddo
  enddo
  !$OMP END PARALLEL
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine computestream

  !> @brief Subroutine for computing new (n+1) vorticity.
  !> The second order derivatives of the Laplacian operator and the first order ones of the RHS of the vorticity equation are
  !> approximated by means of second order central differences and then the resulting finite difference equation is solved by means
  !> of a Successive Over Relaxation (SOR) approach:
  !> \f$v_{i,j}=\beta\left[\frac{v_{i+1,j}+s_{i-1,j}+s_{i,j+1}+s_{i,j-1}}{4}+
  !> Re\frac{\left(s_{i,j+1}-s_{i,j-1}\right)\left(v_{i+1,j}-v_{i-1,j}\right)-
  !> \left(s_{i+1,j}-s_{i-1,j}\right)\left(v_{i,j+1}-v_{i,j-1}\right)}{16}\right]+\left(1-\beta\right)v_{i,j}\f$
  !> @note For the implementation of the boundary conditions see
  !> <a href="http://dx.doi.org/10.1108/EUM0000000004030">T. Stortkuhl, C. Zenger and S. Zimmer</a>.
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
  !$OMP PARALLEL DEFAULT(NONE) &
  !$OMP PRIVATE(ij,i,j)        &
  !$OMP SHARED(cons,Re,beta,dh,N)
  !$OMP DO
  do ij=1,N-1_I4P
    cons(ij,0)%v=(                                                                                    &
                  -(cons(ij-1,1)%s + cons(ij,1)%s + cons(ij+1,1)%s)                   /(3._R8P*dh*dh) &
                  -(  0.5_R8P*cons(ij-1,0)%v +                 0.5_R8P*cons(ij+1,0)%v                 &
                    +0.25_R8P*cons(ij-1,1)%v + cons(ij,1)%v + 0.25_R8P*cons(ij+1,1)%v)/(9._R8P)       &
                 )*9._R8P*0.5_R8P
    cons(ij,N)%v=(-1._R8P/dh                                                                                &
                  -(cons(ij-1,N-1)%s+cons(ij,N-1)%s + cons(ij+1,N-1)%s)                     /(3._R8P*dh*dh) &
                  -(  0.5_R8P*cons(ij-1,N  )%v +                   0.5_R8P*cons(ij+1,N  )%v                 &
                    +0.25_R8P*cons(ij-1,N-1)%v + cons(ij,N-1)%v + 0.25_R8P*cons(ij+1,N-1)%v)/(9._R8P)       &
                 )*9._R8P*0.5_R8P
    cons(0,ij)%v=(                                                                                    &
                  -(cons(1,ij-1)%s + cons(1,ij)%s + cons(1,ij+1)%s)                   /(3._R8P*dh*dh) &
                  -(  0.5_R8P*cons(0,ij-1)%v +                 0.5_R8P*cons(0,ij+1)%v                 &
                    +0.25_R8P*cons(1,ij-1)%v + cons(1,ij)%v + 0.25_R8P*cons(1,ij+1)%v)/(9._R8P)       &
                 )*9._R8P*0.5_R8P
    cons(N,ij)%v=(                                                                                          &
                  -(cons(N-1,ij-1)%s+cons(N-1,ij)%s+cons(N-1,ij+1)%s)                       /(3._R8P*dh*dh) &
                  -(  0.5_R8P*cons(N,ij-1  )%v +                   0.5_R8P*cons(N,ij+1  )%v                 &
                    +0.25_R8P*cons(N-1,ij-1)%v + cons(N-1,ij)%v + 0.25_R8P*cons(N-1,ij+1)%v)/(9._R8P)       &
                 )*9._R8P*0.5_R8P
  enddo
  !$OMP SINGLE
  cons(0,0)%v=(                                                                         &
               -(cons(1,1)%s)/(3._R8P*dh*dh)                                            &
               -(0.5_R8P*cons(1,0)%v+0.5_R8P*cons(0,1)%v+0.25_R8P*cons(1,1)%v)/(9._R8P) &
              )*9._R8P
  cons(N,0)%v=(                                                                             &
               -(cons(N-1,1)%s)/(3._R8P*dh*dh)                                              &
               -(0.5_R8P*cons(N-1,0)%v+0.5_R8P*cons(N,1)%v+0.25_R8P*cons(N-1,1)%v)/(9._R8P) &
              )*9._R8P
  cons(N,N)%v=(-0.5_R8P/dh                                                                      &
               -(cons(N-1,N-1)%s)/(3._R8P*dh*dh)                                                &
               -(0.5_R8P*cons(N-1,N)%v+0.5_R8P*cons(N,N-1)%v+0.25_R8P*cons(N-1,N-1)%v)/(9._R8P) &
              )*9._R8P
  cons(0,N)%v=(-0.5_R8P/dh                                                                  &
               -(cons(1,N-1)%s)/(3._R8P*dh*dh)                                              &
               -(0.5_R8P*cons(1,N)%v+0.5_R8P*cons(0,N-1)%v+0.25_R8P*cons(1,N-1)%v)/(9._R8P) &
              )*9._R8P
  !$OMP END SINGLE
  !$OMP DO COLLAPSE(2)
  do j=1,N-1
    do i=1,N-1
       cons(i,j)%v=beta*((                                                                         &
                          cons(i-1,j)%v+cons(i+1,j)%v+cons(i,j-1)%v+cons(i,j+1)%v                  &
                          -0.25_R8P*Re*(cons(i,j+1)%s-cons(i,j-1)%s)*(cons(i+1,j)%v-cons(i-1,j)%v) &
                          +0.25_R8P*Re*(cons(i+1,j)%s-cons(i-1,j)%s)*(cons(i,j+1)%v-cons(i,j-1)%v) &
                         )*0.25_R8P) + (1._R8P-beta)*cons(i,j)%v
    enddo
  enddo
  !$OMP END PARALLEL
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
  residual%s=0._R8P
  residual%v=0._R8P
  do j=1_I4P,ubound(cons,dim=2)-1_I4P
    do i=1_I4P,ubound(cons,dim=1)-1_I4P
      residual%s =max(residual%s,                                                    &
                      abs((cons(i-1,j)%s-2._R8P*cons(i,j)%s+cons(i+1,j)%s)/(dh*dh) + &
                          (cons(i,j-1)%s-2._R8P*cons(i,j)%s+cons(i,j+1)%s)/(dh*dh) + cons(i,j)%v))
      residual%v =max(residual%v,                                                                               &
                      abs((1._R8P/Re)*(cons(i-1,j)%v-2._R8P*cons(i,j)%v+cons(i+1,j)%v)/(dh*dh)                + &
                          (1._R8P/Re)*(cons(i,j-1)%v-2._R8P*cons(i,j)%v+cons(i,j+1)%v)/(dh*dh)                - &
                          (cons(i,j+1)%s-cons(i,j-1)%s)/(2._R8P*dh)*(cons(i+1,j)%v-cons(i-1,j)%v)/(2._R8P*dh) + &
                          (cons(i+1,j)%s-cons(i-1,j)%s)/(2._R8P*dh)*(cons(i,j+1)%v-cons(i,j-1)%v)/(2._R8P*dh)))
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
  write(unit,'(A)',iostat=err)' s(n)='//str(n=self%s)
  write(unit,'(A)',iostat=err)' v(n)='//str(n=self%v)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction pprint
  !> @}
endmodule Data_Type_Conservative
