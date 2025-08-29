program q1436
    implicit none
    integer,        parameter :: int64 = selected_int_kind(18)
    integer(int64)            :: n

    read *, n
    write(*, '(I0)') find_num(n, 666_int64, 100000000_int64)
    
    contains
        function find_num(seq, num, to) result(pos)
            integer(int64), intent(in) :: seq, num, to
            integer(int64)             :: i, j, pos, prev
            integer(int64)             :: cnt, remainder, divisor
            logical                    :: found
            
            pos    = 0
            cnt    = 0
            prev   = 0
            do i = num, to
                divisor = 1_int64
                found   = .false.
                
                do while (i /= prev)
                    remainder = mod(i, divisor)
                    
                    do while (remainder > 0_int64)
                        if (remainder == num) then
                            found = .true.
                            exit
                        end if
                        remainder = remainder / 10_int64
                    end do
                    
                    if (found) then
                        cnt  = cnt + 1
                        prev = i
                        if (cnt == seq) then
                            pos = i
                            return
                        end if
                    end if
                
                    if (divisor > i) then
                        exit
                    end if
                    divisor = divisor * 10_int64
                end do
            end do
        end function find_num
end program q1436
