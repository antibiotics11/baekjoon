program q11050
        implicit none
        integer, parameter :: int64 = selected_int_kind(18)
        integer(int64) :: n, k, r

        read *, n, k
        r = binomial_coefficient(n, k)
        write(*, '(I0)') r

        contains
                function binomial_coefficient(n, k) result(r)
                        integer(int64), intent(in) :: n, k
                        integer(int64)             :: r

                        r = factorial(n) / (factorial(k) * factorial(n - k))
                end function binomial_coefficient

                recursive function factorial(n) result(r)
                        integer(int64), intent(in) :: n
                        integer(int64)             :: r

                        if (n == 0) then
                                r = 1_int64
                        else if (n > 0) then
                                r = n * factorial(n - 1_int64)
                        else 
                                r = 0_int64
                        end if
                end function factorial
end program q11050
