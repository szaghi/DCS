module IR_Precision
!-----------------------------------------------------------------------------------------------------------------------------------
USE, intrinsic:: ISO_FORTRAN_ENV, only: stdout => OUTPUT_UNIT, stderr => ERROR_UNIT ! Standard output/error logical units.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public:: R8P,  FR8P,  DR8P,  MinR8P,  MaxR8P,  BIR8P,  BYR8P,  smallR8P,  ZeroR8
public:: I4P,  FI4P,  DI4P,  MinI4P,  MaxI4P,  BII4P,  BYI4P
public:: bit_size
public:: str, strz, cton
public:: IR_Init
public:: IR_Print
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
integer, parameter::       R8P=selected_real_kind(15,307) ! 15  digits, range \f$[10^{-307} , 10^{+307}  - 1]\f$; 64 bits.
integer, parameter::       I4P=selected_int_kind(9) ! Range \f$[-2^{31},+2^{31} - 1]\f$, 10 digits plus sign; 32 bits.
character(10), parameter:: FR8P='(E23.15E3)' ! Output format for kind=R8P variable.
integer, parameter::       DR8P=23 ! Number of digits of output format FR8P.
character(5), parameter::  FI4P='(I11)' ! Output format for kind=I4P variable.
character(8), parameter::  FI4PZP='(I11.10)' ! Output format with zero prefixing for kind=I4P variable.
integer, parameter::       DI4P=11 ! Number of digits of output format I4P.
real(R8P), parameter::     MinR8P=-huge(1._R8P ),MaxR8P=huge(1._R8P ) ! Min and max values of kind=R8P variable.
integer(I4P):: BIR8P,      BYR8P ! Number of bits/bytes of kind=R8P variable.
real(R8P), parameter::     smallR8P=tiny(1._R8P ) ! Smallest representable value of kind=R8P variable.
integer(I4P), parameter::  MinI4P=-huge(1_I4P), MaxI4P = huge(1_I4P) ! Min and max values of kind=I4P variable.
integer(I4P), parameter::  BII4P=bit_size(MaxI4P), BYI4P = bit_size(MaxI4P)/8_I4P ! Number of bits/bytes of kind=I4P variable.
real(R8P), parameter::     ZeroR8=nearest(1._R8P, 1._R8P)-nearest(1._R8P,-1._R8P) ! Smallest representable difference of kind=R8P.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
interface bit_size
  module procedure bit_size_R8P
endinterface
interface str
  module procedure strf_R8P,str_R8P,strf_I4P,str_I4P
endinterface
interface strz
  module procedure strz_I4P
endinterface
interface cton
  module procedure ctor_R8P,ctoi_I4P
endinterface
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  elemental function bit_size_R8P(i) result(bits)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  real(R8P), intent(IN):: i       ! Real variable of which number of bits must be computed.
  integer(I4P)::          bits    ! Number of bits of i.
  integer(I4P)::          mold(1) ! "Molding" dummy variable for bits counting.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  bits = size(transfer(i,mold),dim=1,kind=I4P)*8_I4P
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction bit_size_R8P

  elemental function strf_R8P(fm,n) result(str)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN):: fm  ! Format different from the standard for the kind.
  real(R8P),    intent(IN):: n   ! Real to be converted.
  character(DR8P)::          str ! Returned string containing input number.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(str,trim(fm)) n ! Casting of n to string.
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction strf_R8P

  elemental function strf_I4P(fm,n) result(str)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN):: fm  ! Format different from the standard for the kind.
  integer(I4P), intent(IN):: n   ! Integer to be converted.
  character(DI4P)::          str ! Returned string containing input number.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(str,trim(fm)) n ! Casting of n to string.
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction strf_I4P

  elemental function str_R8P(no_sign,n) result(str)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  logical,      intent(IN), optional:: no_sign ! Flag for leaving out the sign.
  real(R8P),    intent(IN)::           n       ! Real to be converted.
  character(DR8P)::                    str     ! Returned string containing input number.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(str,FR8P) n                 ! Casting of n to string.
  if (n>0._R8P) str(1:1)='+'        ! Prefixing plus if n>0.
  if (present(no_sign)) str=str(2:) ! Leaving out the sign.
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction str_R8P

  elemental function str_I4P(no_sign,n) result(str)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  logical,      intent(IN), optional:: no_sign ! Flag for leaving out the sign.
  integer(I4P), intent(IN)::           n       ! Integer to be converted.
  character(DI4P)::                    str     ! Returned string containing input number plus padding zeros.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(str,FI4P) n                 ! Casting of n to string.
  str = adjustl(trim(str))          ! Removing white spaces.
  if (n>=0_I4P) str='+'//trim(str)  ! Prefixing plus if n>0.
  if (present(no_sign)) str=str(2:) ! Leaving out the sign.
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction str_I4P

  elemental function strz_I4P(nz_pad,n) result(str)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P), intent(IN), optional:: nz_pad ! Number of zeros padding.
  integer(I4P), intent(IN)::           n      ! Integer to be converted.
  character(DI4P)::                    str    ! Returned string containing input number plus padding zeros.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(str,FI4PZP) n                              ! Casting of n to string.
  str=str(2:)                                      ! Leaving out the sign.
  if (present(nz_pad)) str=str(DI4P-nz_pad:DI4P-1) ! Leaving out the extra zeros padding
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction strz_I4P

  function ctor_R8P(str,knd) result(n)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN):: str ! String containing input number.
  real(R8P),    intent(IN):: knd ! Number kind.
  real(R8P)::                n   ! Number returned.
  integer(I4P)::             err ! Error trapping flag: 0 no errors, >0 error occurs.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  read(str,*,iostat=err) n ! Casting of str to n.
  if (err/=0) then
    write(stderr,'(A)')            'Conversion of string "'//str//'" to real failed'
    write(stderr,'(A,'//FR8P//')') 'Kind parameter ',knd
    write(stderr,'(A)')            'Function used "ctor_R8P"'
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction ctor_R8P

  function ctoi_I4P(str,knd) result(n)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN):: str ! String containing input number.
  integer(I4P), intent(IN):: knd ! Number kind.
  integer(I4P)::             n   ! Number returned.
  integer(I4P)::             err ! Error trapping flag: 0 no errors, >0 error occurs.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  read(str,*,iostat=err) n ! Casting of str to n.
  if (err/=0) then
    write(stderr,'(A)')            'Conversion of string "'//str//'" to integer failed'
    write(stderr,'(A,'//FI4P//')') 'Kind parameter ',knd
    write(stderr,'(A)')            'Function used "ctoi_I4P"'
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction ctoi_I4P

  subroutine IR_init()
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  BIR8P  = bit_size(i=MaxR8P)  ; BYR8P  = BIR8P/8_I4P
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine IR_init

  subroutine IR_Print()
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call IR_init
  write(stdout,'(A)')             ' Reals kind precision definition'
  write(stdout,'(A,I2,A,I2)')     ' R8P  Kind "',R8P, '" | FR8P  format "'//FR8P// '" | DR8P  chars ',DR8P
  write(stdout,'(A)')             ' Integers kind precision definition'
  write(stdout,'(A,I2,A,I2)')     ' I4P Kind "',I4P,'" | FI4P format "'//FI4P// '" | DI4P chars ',DI4P
  write(stdout,'(A)')             ' Reals minimum and maximum values'
  write(stdout,'(A)')             ' MinR8P  "'//trim(str(n=MinR8P))//'" | MaxR8P  "'//trim(str(n=MaxR8P))//'"'
  write(stdout,'(A)')             ' Reals bits/bytes sizes'
  write(stdout,'(A,I2,A,I2,A)')   ' R8P bits "',BIR8P,'", bytes "',BYR8P,'"'
  write(stdout,'(A)')             ' Integers minimum and maximum values'
  write(stdout,'(A)')             ' MinI4P "'//trim(str(n=MinI4P))//'" | MaxI4P "'//trim(str(n=MaxI4P))//'"'
  write(stdout,'(A)')             ' Integers bits/bytes sizes'
  write(stdout,'(A,I2,A,I2,A)')   ' I4P bits "',BII4P,'", bytes "',BYI4P,'"'
  write(stdout,'(A)')             ' Machine precisions'
  write(stdout,'(A,'//FR8P// ')') ' ZeroR8  "',ZeroR8
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine IR_Print
endmodule IR_Precision
