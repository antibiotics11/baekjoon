module shared_data
        implicit none
        integer,        parameter :: int64   = selected_int_kind(18)
        integer(int64), parameter :: MAX_N   = 1000_int64
        integer(int64), parameter :: DIVISOR = 10007_int64

        contains
                subroutine index_exists(i, j, arr)
                        integer(int64), intent(in) :: i, j
                        integer(int64), intent(in) :: arr(:,:)
                        if (i < lbound(arr, 1) .or. i > ubound(arr, 1) .or. &
                        j < lbound(arr, 2) .or. j > ubound(arr, 2)) then
                                write(*, '("undefined index: [", I0, "][", I0, "]")') i, j
                                stop 1
                        end if
                end subroutine index_exists
end module shared_data

program q11051
        use shared_data
        implicit none
        integer(int64) :: n, k

        read *, n, k
        write(*, '(I0)') remainder(n, k, DIVISOR)

        contains
                recursive function remainder(n, k, divisor) result(r)
                        integer(int64), save       :: cache(0:MAX_N, 0:MAX_N) = -1_int64
                        integer(int64), intent(in) :: n, k, divisor
                        integer(int64)             :: r

                        if (k == 0_int64 .or. n == k) then
                                cache(n, k) = 1_int64
                                r = cache(n, k)
                                return
                        end if

                        call index_exists(n, k, cache)

                        if (cache(n, k) == -1_int64) then
                                cache(n, k) = MOD( (                        &
                                        remainder(n - 1, k - 1, divisor) +  &
                                        remainder(n - 1, k,     divisor)    &
                                        ), divisor )
                        end if

                        r = cache(n, k)
                end function remainder
end program q11051
