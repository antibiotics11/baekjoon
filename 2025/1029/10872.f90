program q10872
    use, intrinsic :: iso_fortran_env, &
        only: input_unit, output_unit, int32

    call program_main()
contains
    subroutine program_main()
        integer(int32) :: n

        read(input_unit, *) n
        write(output_unit, "(I0)") factorial(n)
    end subroutine program_main

    integer(int32) function factorial(n)
        integer(int32), intent(in) :: n
        integer(int32)             :: i

        factorial = 1
        do i = 1, n
            factorial = factorial * i
        end do
    end function factorial
end program q10872
