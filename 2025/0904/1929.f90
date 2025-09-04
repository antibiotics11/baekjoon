program q1929
    use, intrinsic :: iso_fortran_env, &
        only: int64, INPUT_UNIT, OUTPUT_UNIT
    implicit none

    call program_main()

contains
    subroutine program_main()
        intrinsic :: int, real, sqrt, achar
        integer(int64), parameter :: MAX = 1000000
        integer(int64) :: m, n, i, j, limit
        logical        :: is_prime(0:MAX + 1)

        read(INPUT_UNIT, *) m, n

        is_prime    = .true.
        is_prime(0) = .false.
        is_prime(1) = .false.
        limit = int(sqrt(real(MAX, kind=8)))

        do i = 1, limit
            if (is_prime(i)) then
                do j = i * i, MAX + 1, i
                    is_prime(j) = .false.
                end do
            end if
        end do

        do i = m, n
            if (.not. is_prime(i)) then
                cycle
            end if
            write(OUTPUT_UNIT, '(I0, A)', advance='no') i, achar(10)
        end do
    end subroutine program_main
end program q1929
