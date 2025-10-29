program q15439
    use, intrinsic :: iso_fortran_env, &
        only: input_unit, output_unit, int16

    call program_main()
contains
    subroutine program_main()
        integer(int16) :: n

        read(input_unit, *) n
        write(output_unit, "(I0)") n * (n - 1)
    end subroutine program_main
end program q15439
