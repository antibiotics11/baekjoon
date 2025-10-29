program q11050
    use, intrinsic :: iso_fortran_env, &
        only: input_unit, output_unit, int32

    call program_main()
contains
    subroutine program_main()
        integer(int32) :: n, k

        read(input_unit, *) n, k
        write(output_unit, "(I0)") binomial_coefficient(n, k)
    end subroutine program_main

    integer(int32) function binomial_coefficient(n, r)
        integer(int32), intent(in) :: n, r
        binomial_coefficient = factorial(n) / (factorial(r) * factorial(n - r))
    end function binomial_coefficient

    integer(int32) function factorial(n)
        integer(int32), intent(in) :: n
        integer(int32)             :: i

        factorial = 1
        do i = 1, n
            factorial = factorial * i
        end do
    end function factorial
end program q11050
