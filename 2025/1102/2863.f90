program q2863
    use, intrinsic :: iso_fortran_env, &
        only: input_unit, output_unit, int8, real32
    implicit none

    call program_main()
contains
    subroutine program_main()
        real(real32)  :: table(2,2)
        real(real32)  :: tmp_sum, max_sum = -1
        integer(int8) :: i, max_cnt

        read(input_unit, *) table(1,:)
        read(input_unit, *) table(2,:)
        do i = 0, 3
            tmp_sum = table_sum(table)
            if (tmp_sum > max_sum) then
                max_sum = tmp_sum
                max_cnt = i
            end if
            call table_rotate(table)
        end do
        write(output_unit, "(I0)") max_cnt
    end subroutine program_main

    subroutine table_rotate(table)
        real(real32), intent(inout) :: table(2,2)
        real(real32)                :: tmp
        tmp = table(1,1)
        table(1,1) = table(2,1)
        table(2,1) = table(2,2)
        table(2,2) = table(1,2)
        table(1,2) = tmp
    end subroutine table_rotate

    real(real32) function table_sum(table)
        real(real32), intent(in) :: table(2,2)
        table_sum = table(1,1) / table(2,1) + &
                    table(1,2) / table(2,2)
    end function table_sum
end program q2863
