program q10872
        implicit none
        integer(kind=8) :: n, r

        read *, n
        r = factorial(n)
        write(*, '(I0)') r

        contains
                recursive function factorial(n) result(r)
                        integer(kind=8), intent(in) :: n
                        integer(kind=8)             :: r
                        if (n > 0) then
                                r = n * factorial(n - 1)
                        else if (n == 0) then
                                r = 1
                        end if
                end function factorial
end program q10872
