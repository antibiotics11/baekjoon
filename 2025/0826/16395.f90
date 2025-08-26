program q16395
        implicit none
        integer,        parameter :: int64 = selected_int_kind(18)
        integer(int64), parameter :: MAX_N = 500_int64
        integer(int64)            :: n, k, r

        read *, n, k
        r = triangle(n, k)
        write(*, '(I0)') r

        contains
                recursive function triangle(n, k) result(r)
                        integer(int64), save       :: arr(MAX_N) = 0_int64
                        integer(int64), intent(in) :: n, k
                        integer(int64)             :: arr_key
                        integer(int64)             :: top_right, top_left
                        integer(int64)             :: r

                        top_right = 0
                        top_left  = 0
                        arr_key   = (n * (n + 1_int64)) / 2_int64 - n + k

                        if (arr_key < lbound(arr, 1) .or. arr_key > ubound(arr, 1)) then
                                write(*, '("undefined index: ", I0)') arr_key
                                stop 1
                        end if

                        if (n == 1_int64) then
                                arr(arr_key) = 1_int64
                        end if

                        if (arr(arr_key) == 0_int64) then
                                if (k > 1_int64) then
                                        top_left = triangle(n - 1_int64, k - 1_int64)
                                end if
                                if (k < n) then
                                        top_right = triangle(n - 1_int64, k)
                                end if

                                arr(arr_key) = top_right + top_left
                        end if

                        r = arr(arr_key)
                end function triangle
end program q16395
