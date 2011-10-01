!>
!! @file   list.f90
!! @author Pawel Biernat <pawel.biernat@gmail.com>
!! @date   Sat Sep 24 17:12:18 2011
!!
!! @brief Implementation of a list as an expandable container for a
!! polymorphic data type.
!!
!!
!!
module class_list

  abstract interface
     subroutine call_subroutine_interface(this)
       class(*) :: this
     end subroutine call_subroutine_interface
  end interface

  private

  type, public :: list
     class(*), pointer :: element => null()
     type(list), pointer :: next => null()
   contains
     procedure :: map => map_subroutine_on_list
     procedure :: last
     procedure :: add
     procedure :: length
     procedure :: take
  end type list

contains

  function take(this, i) result (element)
    class(list), intent(in), target :: this
    class(list), pointer :: r
    class(*), pointer :: element
    integer, intent(in) :: i
    integer :: j
    nullify(element)
    nullify(r)

    if( i > this % length() ) then
       return
    else
       r => this
       j = 1
       do while( j < i )
          r => r % next
          j = j + 1
       end do
       element => r % element
    end if

  end function take


  !> Function used as a wrapper to call f on class(*) object.
  !!
  !! @param this
  !! @param f
  !!
  !! @return
  !!
  subroutine call_f_on_this( this, f )
    class(*) :: this
    procedure(call_subroutine_interface) :: f

    call f(this)
  end subroutine call_f_on_this

  !> Function similar to call_f_on_element() but calls f for every
  !> element of the list
  !!
  !! @param this first element of the list
  !! @param f function to call
  !!
  !! @return
  !!
  recursive subroutine map_subroutine_on_list( this, f )
    class(list), target, intent(in) :: this
    procedure(call_subroutine_interface) :: f
    class(list), pointer :: current

    ! if list has no elements yet
    if( this % length() == 0 ) then
       return
    else
       current => this
       call f( current % element )
       do while( associated( current % next ) )
          current => current % next
          ! call f on element, condition is just a precaution
          if( associated( current % element ) ) then
             call f(current % element)
          else
             print *, "list element is null"
          end if
       end do
    end if
  end subroutine map_subroutine_on_list

  !> Adds an element to the end of the list
  !!
  !! @param this
  !! @param element
  !!
  !! @return
  !!
  subroutine add( this, element )
    class(list) :: this
    class(*), pointer, intent(in) :: element
    class(list), pointer :: holder, l
    l => this % last()

    ! a special case when a list "this" was just initialized and
    ! consists of only one list element and this element does not
    ! have any content
    if( .not. associated( l ) ) then
       this % element => element
       return
    else
       ! we know that l is not null, as we checked that above
       allocate(holder)
       holder % element => element
       nullify( holder % next )
       l % next => holder
    end if

  end subroutine add

  !> returns a pointer to the last element of the list, if the list
  !> does not have any elements it returns null
  !!
  !! @param this a list
  !!
  !! @return pointer to a last element
  !!
  function last( this ) result(l)
    class(list), target, intent(in) :: this
    class(list), pointer :: l

    ! "this" is the first and last element, so the function shall
    ! return a pointer to "this"
    if( .not. associated( this % element )) then
       nullify(l)
       return
    else
       ! otherwise we choose a pointer to a first element of the list
       l => this
       ! and follow it to the last element
       do while( associated( l % next ) )
          l => l % next
       end do
    end if

  end function last

  function length( this ) result(i)
    class(list), target, intent(in) :: this
    integer :: i
    class(list), pointer :: l
    i = 0
    l => this

    ! list has been just initialized, and has zero elements
    if( .not. associated( l % element ) ) then
       return
    else
       ! there is at least one element
       i = 1
       ! and possibly more
       do while( associated( l % next ) )
          i = i + 1
          l => l % next
       end do
    end if


  end function length


end module class_list
