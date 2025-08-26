program q15439
        implicit none
        integer(kind=8) :: n, r

        read *, n
        r = n * (n - 1)
        write(*, "(I0)") r
end program q15439
