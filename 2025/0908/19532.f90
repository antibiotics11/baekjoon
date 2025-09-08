program q19532
    use, intrinsic :: iso_fortran_env, &
        only: real32, input_unit, output_unit
    implicit none

    call program_main()

contains
    subroutine program_main()
        real(real32) :: a, b, c, d, e, f, x, y, det

        read(input_unit, *) a, b, c, d, e, f

        det = (a * e - b * d)
        x   = (c * e - b * f) / det
        y   = (a * f - c * d) / det

        write(output_unit, "(I0, ' ', I0)") int(x), int(y)
    end subroutine program_main
end program q19532
