program q2839
    implicit none
    integer, parameter :: int64 = selected_int_kind(18)
    integer(int64)     :: n
    integer(int64)     :: divisor, quotient, remainder, combination, tmp, i
    
    read *, n
    
    combination = n
    do i = 0, n
        quotient  = n / 5 - i
        remainder = 5 * i + mod(n, 5)
        
        if (quotient < 0 .or. remainder < 0) then
            exit
        end if
        
        if (remainder == 0) then
            combination = quotient
            exit
        end if
        
        if (mod(remainder, 3) == 0) then
            tmp = quotient + remainder / 3
            if (tmp < combination) then
                combination = tmp
            end if
        end if
    end do
    
    if (combination == n) then
        combination = -1
    end if

    write(*, '(I0)') combination
end program q2839
