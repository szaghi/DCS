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

!> @ingroup Program
!> @{
!> @defgroup DCSProgram DCS
!> Driven-Cavity Simulator
!> @}

!> @ingroup PrivateVarPar
!> @{
!> @defgroup DCSPrivateVarPar DCS
!> Driven-Cavity Simulator
!> @}

!> @ingroup PrivateProcedure
!> @{
!> @defgroup DCSPrivateProcedure DCS
!> Driven-Cavity Simulator
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
type(Type_Cavity):: cavity            !< Cavity data.
real(R8P)::         rtol = 0.0001_R8P !< Residual tolerance.
integer(I4P)::      nout = 1000_I4P   !< Console updating frequency.
character(3)::      oform = "tec"     !< Output file format: "tec"=> Tecplot Inc. , "gnu"=> gnuplot.
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
  write(stdout,'(A)')' ./DCS -N number_of_cells [optionals parameters]'
  write(stdout,*)
  write(stdout,'(A)')' Optional parameters:'
  write(stdout,'(A)')' -Re Reynolds_number => default 500'
  write(stdout,'(A)')' -beta over_relaxation_parameter => default 0.6'
  write(stdout,'(A)')' -rtol residual_tolerance => default 10^-4'
  write(stdout,'(A)')' -nout standard_output_update_frequency => default 1000 iterations'
  write(stdout,'(A)')' -oform output_file_format => "tec" for Tecplot output or "gnu" for gnuplot one, default "tec"'
  write(stdout,*)
  write(stdout,'(A)')' Example: '
  write(stdout,'(A)')' ./DCS -Re 9.d2 -beta 0.5d0 -N 256 -nout 10000 -rtol 1.d-6'
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
  character(6)::   ca_switch !< Switch identifier.
  character(100):: sbuf      !< String buffer.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  Re = cavity%Re
  beta = cavity%beta
  N = cavity%mesh%N
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
      case('-oform')
        call get_command_argument(c+1,oform) ; c = c + 1
      case default
        write(stderr,'(A)') ' Error: switch "'//adjustl(trim(ca_switch))//'" is unknown'
        call print_usage
        stop
      endselect
    enddo
  endif
  if (N==0_I4P) then
    write(stderr,'(A)')' Error: the number of cells N must be passed as command line argument'
    call print_usage
    stop
  endif
  write(stdout,'(A)')' Some information about the precision of runnig machine'
  call IR_Print()
  write(stdout,*)
  write(stdout,'(A)')' Simulation parameters'
  write(stdout,'(A)')' Re='//trim(str(n=Re))
  write(stdout,'(A)')' beta='//trim(str(n=beta))
  write(stdout,'(A)')' N='//trim(str(n=N))
  write(stdout,'(A)')' rtol='//trim(str(n=rtol))
  write(stdout,'(A)')' nout='//trim(str(n=nout))
  write(stdout,'(A)')' oform='//trim(oform)
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
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  ! successive over-relaxation (SOR) loop
  call residual%set(s=MaxR8P,v=MaxR8P)
  SOR: do while(((residual%s>rtol).AND.(residual%v>rtol)))
    ! updating time step counter
    n = n + 1_I4P
    ! computing new stream function
    call computestream(cons=cavity%cons,beta=cavity%beta,dh=cavity%mesh%dh)
    ! computing new vorticity
    call computevorticity(cons=cavity%cons,Re=cavity%Re,beta=cavity%beta,dh=cavity%mesh%dh)
    ! computing residuals
    residual = computeresidual(cons=cavity%cons,Re=cavity%Re,dh=cavity%mesh%dh)
    if (mod(n,nout)==0) then
      write(stdout,'(A)')' iteration n='//trim(str(n=n))//&
                         ' residuals: s='//trim(str(n=residual%s))//' v='//trim(str(n=residual%v))
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
  select case(trim(oform))
  case('tec')
    ! tecplot format
    open(unit=Get_Unit(u),file='DCS_out.tec')
    write(u,'(A)')'VARIABLES="x" "y" "s" "v"'
    write(u,'(A)')'ZONE T="Driven Cavity" I='//trim(str(n=cavity%mesh%N+1))//' J='//trim(str(n=cavity%mesh%N+1))
    do j=0,cavity%mesh%N
      do i=0,cavity%mesh%N
        write(u,'(A)')trim(str(n=cavity%mesh%x(i)))//' '//trim(str(n=cavity%mesh%y(j)))//' '//&
                      trim(str(n=cavity%cons(i,j)%s))//' '//trim(str(n=cavity%cons(i,j)%v))
      enddo
    enddo
    close(u)
  case('gnu')
    ! gnuplot format
    open(unit=Get_Unit(u),file='DCS_out.gnu')
    do j=0,cavity%mesh%N
      do i=0,cavity%mesh%N
        write(u,'(A)')trim(str(n=cavity%mesh%x(i)))//' '//trim(str(n=cavity%mesh%y(j)))//' '//&
                      trim(str(n=cavity%cons(i,j)%s))//' '//trim(str(n=cavity%cons(i,j)%v))
      enddo
      write(u,*)
    enddo
    close(u)
  endselect
  ! finalizing cavity data
  call cavity%finalize
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine dcs_finalize
  !> @}
endprogram DCS
