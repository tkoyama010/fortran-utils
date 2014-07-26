module misc
implicit none
contains
elemental function factorial(n) result(val)
  implicit none
  integer,intent(in) :: n
  integer :: k 
  integer :: val
  val = 1
  do k = 1, n
    val = val*k
  end do
end function
end module

