module function_base

use types, only: dp
use constants, only: pi, i_
use utils, only: stop_error
use optimize, only: bisect
use amos
implicit none

private
public angle

interface angle
  module procedure angle_real
  module procedure angle_complex
end interface angle

contains

elemental pure function angle_real(z, deg) result(res)
  implicit none
  real(dp) :: res
  real(dp),intent(in) :: z
  integer,optional,intent(in) :: deg
  double precision fact
  real(dp) :: zimag, zreal
  if(present(deg))then
    if(deg > 0.d0)then
        fact = 180.d0/pi
    else
        fact = 1.d0
    end if
  else
      fact = 1.d0
  endif
  zimag = 0.d0
  zreal = z  
  res = atan2(zimag, zreal) * fact
end function angle_real

elemental pure function angle_complex(z, deg) result(res)
  implicit none
  real(dp) :: res
  complex(dp*2),intent(in) :: z
  integer,optional,intent(in) :: deg
  double precision fact
  real(dp) :: zimag, zreal
  if(present(deg))then
    if(deg > 0.d0)then
        fact = 180.d0/pi
    else
        fact = 1.d0
    end if
  else
      fact = 1.d0
  endif
  zimag = aimag(z)
  zreal = real(z)
  res = atan2(zimag, zreal) * fact
end function angle_complex

end module

