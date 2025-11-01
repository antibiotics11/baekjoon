program q1010
    use, intrinsic :: iso_fortran_env, &
        only: input_unit, output_unit, int64
    implicit none

    call program_main()
contains
    subroutine program_main()
        integer(int64) :: t
        integer(int64) :: m, n

        read(input_unit, *) t
        do while (t > 0)
            read(input_unit, *) m, n
            write(output_unit, "(I0)") cases(n, m)
            t = t - 1
        end do
    end subroutine program_main

    integer(int64) function cases(n, k)
        intrinsic :: mod
        integer(int64), intent(in)  :: n, k
        integer(int64)              :: i, j
        integer(int64), allocatable :: dp(:)

        allocate(dp(size_of(n + 1)))
        dp = 0

        do i = 1, n + 1
            do j = 1, i
                if (j == i .or. j == 1) then
                    dp(index_of(i, j)) = 1
                    cycle
                end if
                dp(index_of(i, j)) = &
                    dp(index_of(i - 1, j - 1)) + &
                    dp(index_of(i - 1, j))
            end do
        end do
        cases = dp(index_of(n + 1, k + 1))
    end function cases

    integer(int64) function index_of(n, k)
        integer(int64), intent(in) :: n, k
        index_of = size_of(n - 1) + k
    end function index_of

    integer(int64) function size_of(n)
        integer(int64), intent(in) :: n
        size_of = n * (n + 1) / 2
    end function size_of
end program q1010
