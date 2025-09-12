program q9613
    use, intrinsic :: iso_fortran_env, &
        only: int8, int32, int64, input_unit, output_unit
    implicit none

    call program_main()

contains
    subroutine program_main()
        integer(int8)  :: t, n
        integer(int32) :: l(100)

        read(input_unit, *) t
        do while (t > 0)
            l = 0
            read(input_unit, *) n, l(1:n)
            write(output_unit, "(I0)") GCD_sum(n, l)

            t = t - 1
        end do
    end subroutine program_main

    integer(int64) function GCD_sum(n, l)
        integer(int8),  intent(in) :: n
        integer(int32), intent(in) :: l(:)
        integer(int32)             :: i, j

        GCD_sum = 0
        do i = 1, n - 1
            do j = i + 1, n
                GCD_sum = GCD_sum + GCD(l(i), l(j))
            end do
        end do
    end function GCD_sum

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
end program q9613
