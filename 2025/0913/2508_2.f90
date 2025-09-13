program q2508
    use, intrinsic :: iso_fortran_env, &
        only: input_unit, output_unit, int16
    implicit none

    call program_main()

contains
    subroutine program_main()
        integer(int16)                :: t, r, c, i, j
        character(len=1), allocatable :: matrix(:,:)
        character(len=:), allocatable :: line

        read(input_unit, *) t
        do while (t /= 0)
            read(input_unit, "(A)")
            read(input_unit, *) r, c

            allocate(matrix(r,c))
            allocate(character(len=c) :: line)

            do i = 1, r
                read(input_unit, "(A)") line
                do j = 1, c
                    matrix(i,j) = line(j:j)
                end do
            end do            
            write(output_unit, "(I0)") candies(matrix, r, c)

            deallocate(matrix, line)

            t = t - 1
        end do
    end subroutine program_main

    integer(int16) function candies(matrix, r, c)
        character(len=1), intent(in) :: matrix(:,:)
        integer(int16),   intent(in) :: r, c
        integer(int16)               :: x, y

        candies = 0
        do y = 1, r
            do x = 1, c
                if (matrix(y,x) == ">" .and. x <= c - 2) then
                    if (matrix(y,x + 1) == "o" .and. &
                        matrix(y,x + 2) == "<") then
                        candies = candies + 1
                        cycle
                    end if
                end if
                if (matrix(y,x) == "v" .and. y <= r - 2) then
                    if (matrix(y + 1,x) == "o" .and. &
                        matrix(y + 2,x) == "^") then
                        candies = candies + 1
                        cycle
                    end if
                end if
            end do
        end do
    end function candies
end program q2508
