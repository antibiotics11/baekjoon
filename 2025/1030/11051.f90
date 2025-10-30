program q11051
    implicit none

    call program_main()
contains
    subroutine program_main()
        use, intrinsic :: iso_fortran_env, &
            only: input_unit, output_unit, int64
        integer(int64), parameter :: divisor = 10007
        integer(int64)            :: n, k

        read(input_unit, *) n, k
        write(output_unit, "(I0)") remainder(n, k, divisor)
    end subroutine program_main

    integer(int64) function remainder(n, k, divisor)
        use, intrinsic :: iso_fortran_env, only: int64
        intrinsic :: mod
        integer(int64), intent(in)  :: n, k, divisor
        integer(int64)              :: i, j
        integer(int64), allocatable :: remainders(:)

        allocate(remainders(size_of(n + 1)))
        remainders = 0

        do i = 1, n + 1
            do j = 1, i
                if (j == 1 .or. i == j) then
                    remainders(index_of(i, j)) = 1_int64
                    cycle
                end if
                remainders(index_of(i, j)) =                 & 
                    mod((                                    &
                        remainders(index_of(i - 1, j - 1)) + &
                        remainders(index_of(i - 1, j))       &
                    ), divisor)
            end do
        end do
        remainder = remainders(index_of(n + 1, k + 1))
    end function remainder

    integer(int64) function index_of(n, k)
        use, intrinsic :: iso_fortran_env, only: int64
        integer(int64), intent(in) :: n, k
        index_of = size_of(n - 1) + k
    end function index_of

    integer(int64) function size_of(n)
        use, intrinsic :: iso_fortran_env, only: int64
        integer(int64), intent(in) :: n
        size_of = n * (n + 1_int64) / 2_int64
    end function size_of
end program q11051
