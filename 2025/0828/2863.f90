program q2863
    implicit none
    real(kind=8) :: table(4)
    real(kind=8) :: exp_result, max_result
    integer      :: i, rotate_cnt

    read *, table(1), table(2)
    read *, table(3), table(4)

    rotate_cnt = 0
    max_result = 0

    do i = 1, 4
        exp_result = table(1) / table(3) + table(2) / table(4)
        if (exp_result > max_result) then
            max_result = exp_result
            rotate_cnt = i - 1
        end if
        table = rotate(table)
    end do

    write(*, '(I0)') rotate_cnt

    contains
        function rotate(table) result(new_table)
            real(kind=8), intent(in) :: table(4)
            real(kind=8)             :: new_table(4)

            new_table(1) = table(3)
            new_table(2) = table(1)
            new_table(3) = table(4)
            new_table(4) = table(2)
        end function rotate
end program q2863
