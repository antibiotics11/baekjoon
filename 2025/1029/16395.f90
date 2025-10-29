program q16395
    use, intrinsic :: iso_fortran_env, &
        only: input_unit, output_unit, int64

    call program_main()
contains
    subroutine program_main()
        integer(int64) :: n, k
        integer(int64), allocatable :: triangle(:)

        read(input_unit, *) n, k
        triangle = pascals_triangle(n)
        write(output_unit, "(I0)") triangle(index_of(n, k))
    end subroutine program_main

    function pascals_triangle(n) result(triangle)
        integer(int64), intent(in)  :: n
        integer(int64), allocatable :: triangle(:)
        integer(int64)              :: i, j
        integer(int64)              :: prev1, prev2

        allocate(triangle(size_of(n)))
        triangle = 0
        triangle(1) = 1

        do i = 2, n
            do j = 1, i
                if (j == 1) then
                    prev2 = 0
                else
                    prev2 = triangle(index_of(i - 1, j - 1))
                end if
                if (j == i) then
                    prev1 = 0
                else
                    prev1 = triangle(index_of(i - 1, j))
                end if

                triangle(index_of(i, j)) = prev1 + prev2
            end do
        end do
    end function pascals_triangle

    integer(int64) function index_of(n, k)
        integer(int64), intent(in) :: n, k
        index_of = size_of(n - 1) + k
    end function index_of

    integer(int64) function size_of(n)
        integer(int64), intent(in) :: n
        size_of = n * (n + 1) / 2
    end function size_of
end program q16395
