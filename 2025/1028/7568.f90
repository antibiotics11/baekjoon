module body_mod
    use, intrinsic :: iso_fortran_env, only: int16

    type :: body_build
        integer(int16) :: weight = 0, &
                          height = 0
    contains
        procedure :: smaller_than
    end type body_build

contains
    logical function smaller_than(self, other)
        class(body_build), intent(in) :: self, other
        smaller_than = &
            (self%weight < other%weight .and. &
             self%height < other%height)
    end function smaller_than
end module body_mod

program q7568
    use            :: body_mod
    use, intrinsic :: iso_fortran_env, &
        only: input_unit, output_unit, int8
    implicit none

    call program_main()
contains
    subroutine program_main()
        type(body_build), allocatable :: people(:)
        integer(int8),    allocatable :: rank(:)
        integer(int8)                 :: n, i, j

        read(input_unit, *) n

        allocate(people(n), rank(n))
        rank = 1

        do i = 1, n
            read(input_unit, *) people(i)%weight, people(i)%height
        end do
        do i = 1, n
            do j = 1, n
                if (i == j) then
                    cycle
                end if
                if (people(i)%smaller_than(people(j))) then
                    rank(i) = rank(i) + 1_int8
                end if
            end do
        end do
        do i = 1, n
            write(output_unit, "(I0, ' ')", advance="no") rank(i)
        end do
    end subroutine program_main
end program q7568
