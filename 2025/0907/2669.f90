program q2669
    use, intrinsic :: iso_fortran_env, &
        only: int8, int16, input_unit, output_unit
    implicit none

    call program_main()

contains
    subroutine program_main()
        logical       :: matrix(0:100, 0:100)
        integer(int8) :: bl_x, bl_y, tr_x, tr_y
        integer(int8) :: i

        matrix = .false.

        do i = 1, 4
            read(input_unit, *) bl_x, bl_y, tr_x, tr_y
            call fill_matrix(matrix, bl_x, bl_y, tr_x, tr_y)
        end do

        write(output_unit, "(I0)") area(matrix)
    end subroutine program_main

    subroutine fill_matrix(matrix, bl_x, bl_y, tr_x, tr_y)
        logical,       intent(inout) :: matrix(:, :)
        integer(int8), intent(in)    :: bl_x, bl_y, tr_x, tr_y
        integer(int8)                :: x, y

        do y = 1, tr_y - 1
            do x = 1, tr_x - 1
                if ((bl_y <= y .and. y < tr_y) .and. &
                    (bl_x <= x .and. x < tr_x)) then
                    matrix(y, x) = .true.
                end if
            end do
        end do
    end subroutine fill_matrix

    integer(int16) function area(matrix)
        intrinsic :: size
        logical,      intent(in) :: matrix(:, :)
        integer(int8)            :: x, y

        area   = 0
        do y = 1, size(matrix, 1) - 1
            do x = 1, size(matrix, 2) - 1
                if (matrix(y, x)) then
                    area = area + 1
                end if
            end do
        end do
    end function area

end program q2669
