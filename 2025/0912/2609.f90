program q2609
    use, intrinsic :: iso_fortran_env, &
        only: int32, input_unit, output_unit
    implicit none

    call program_main()

contains
    subroutine program_main()
        integer(int32) :: a, b
        read(input_unit, *) a, b
        write(output_unit, "(I0)") GCD(a, b)
        write(output_unit, "(I0)") LCM(a, b)
    end subroutine program_main

    integer(int32) function LCM(a, b)
        integer(int32), intent(in) :: a, b
        LCM = (a / GCD(a, b)) * b
    end function LCM

    integer(int32) function GCD(a, b)
        integer(int32), intent(in) :: a, b
        integer(int32)             :: tmp, tmp_a, tmp_b

        if (b == 0) then
            GCD = a
            return
        end if

        tmp_a = a
        tmp_b = b
        do while (tmp_b /= 0)
            tmp   = mod(tmp_a, tmp_b)
            tmp_a = tmp_b
            tmp_b = tmp
        end do
        GCD = tmp_a
    end function GCD
end program q2609
