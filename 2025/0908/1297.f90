program q1297
    use, intrinsic :: iso_fortran_env, &
        only: real32, int16, input_unit, output_unit
    implicit none
    intrinsic :: sqrt, floor

    call program_main()

contains
    subroutine program_main()
        real(real32) ::   width,   height,   diagonal, &
                        r_width, r_height, r_diagonal, &
                        scale_factor

        read(input_unit, *) diagonal, r_height, r_width

        r_diagonal   = sqrt(r_width ** 2 + r_height ** 2)
        scale_factor = diagonal / r_diagonal
        width        = r_width * scale_factor
        height       = r_height * scale_factor

        write(output_unit, "(I0, ' ', I0)") floor(height), floor(width)
    end subroutine program_main
end program q1297
