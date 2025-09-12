program q1934
    use, intrinsic :: iso_fortran_env, &
        only: int32, input_unit, output_unit
    implicit none

    call program_main()

contains
    subroutine program_main()
        integer(int32) :: a, b, t, i

        read(input_unit, *) t
        do i = 1, t
            read(input_unit, *) a, b
            write(output_unit, "(I0)") LCM(a, b)
        end do
    end subroutine program_main

    integer(int32) function LCM(a, b)
        integer(int32), intent(in) :: a, b
        LCM = (a / GCD(a, b)) * b
    end function LCM

    recursive function GCD(a, b) result(g)
        intrinsic :: mod
        integer(int32), intent(in) :: a, b
        integer(int32)             :: g

        if (b == 0) then
            g = a
        else
            g = GCD(b, mod(a, b))
        end if
    end function GCD
end program q1934
