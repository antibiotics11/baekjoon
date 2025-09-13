program q1094
    use, intrinsic :: iso_fortran_env, &
        only: int8, input_unit, output_unit
    implicit none

    call program_main()

contains
    subroutine program_main()
        intrinsic :: bit_size, btest
        integer(int8) :: x, i, cnt

        read(input_unit, *) x

        cnt = 0
        do i = 0, bit_size(x) - 1
            if (btest(x, i)) then
                cnt = cnt + 1
            end if
        end do

        write(output_unit, "(I0)") cnt
    end subroutine program_main
end program q1094
