program q3460
    use, intrinsic :: iso_fortran_env, &
        only: input_unit, output_unit, int8, int32
    implicit none

    call program_main()

contains
    subroutine program_main()
        intrinsic :: bit_size, btest
        integer(int8)  :: t, i
        integer(int32) :: n

        read(input_unit, *) t
        do while (t > 0)
            n = 0
            read(input_unit, *) n
            do i = 0, bit_size(n) - 1
                if (btest(n, i)) then
                    write(output_unit, "(I0, ' ')", advance = "no") i
                end if
            end do
            write(output_unit, *)
            t = t - 1
        end do
    end subroutine program_main
end program q3460
