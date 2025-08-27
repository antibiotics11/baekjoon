program q1010
        implicit none
        integer,        parameter   :: int64    = selected_int_kind(18)
        integer(int64), parameter   :: MAX_SIZE = 100_int64
        integer(int64)              :: n, m, t, i
        integer(int64), allocatable :: cases(:)

        read *, t
        allocate(cases(t))
        do i = 1, t
                read *, n, m
                cases(i) = num_of_cases(m, n)
        end do
        do i = 1, t
                write(*, '(I0)') cases(i)
        end do
        deallocate(cases)

        contains
                recursive function num_of_cases(n, k) result(r)
                        integer(int64), save       ::cache(0:MAX_SIZE, 0:MAX_SIZE) = -1_int64
                        integer(int64), intent(in) :: n, k
                        integer(int64)             :: r

                        if (k == 0_int64 .or. n == k) then
                                cache(n, k) = 1_int64
                        end if
                        if (cache(n, k) == -1_int64) then
                                cache(n, k) = num_of_cases(n - 1, k - 1) + num_of_cases(n - 1, k)
                        end if

                        r = cache(n, k)
                end function num_of_cases
end program q1010
