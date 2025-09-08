program q11068
    use, intrinsic :: iso_fortran_env, &
        only: int8, int32, input_unit, output_unit
    implicit none

    call program_main()

contains
    subroutine program_main()
        intrinsic :: len_trim
        integer(int8)     :: cases, num_mid, i, b
        integer(int32)    :: num_int, num_len
        character(len=20) :: num_str, num_rev
        logical           :: is_palindrome

        read(input_unit, *) cases
        do i = 1, cases
            read(input_unit, *) num_int
            is_palindrome = .false.

            do b = 2, 64
                num_str = basestr(num_int, b)
                num_len = len_trim(num_str)
                num_rev = strrev(num_str, num_len)
                num_mid = num_len / 2

                if (num_str(1:num_mid) == num_rev(1:num_mid)) then
                    is_palindrome = .true.
                end if
            end do
            if (is_palindrome) then
                write(output_unit, "('1')")
                cycle
            end if
            write(output_unit, "('0')")
        end do

    end subroutine program_main

    ! 특정 진법으로 (거꾸로) 변환하기
    function basestr(num, base) result(buffer)
        intrinsic :: mod
        integer(int32),    intent(in) :: num
        integer(int8),     intent(in) :: base
        integer(int32)                :: digit, tmp, i
        character(len=20)             :: buffer
        character(len=64), parameter  :: symbols = &
            "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ&
            &abcdefghijklmnopqrstuvwxyz+/"

        i      = 0
        tmp    = num
        buffer = ""
        do while (tmp > 0)
            digit = mod(tmp, base)
            i = i + 1
            buffer(i:i) = symbols(digit + 1:digit + 1)
            tmp = tmp / base
        end do
    end function basestr

    ! 문자열 뒤집기
    function strrev(string, length) result(reversed)
        character(len=*), intent(in)  :: string
        character(len=:), allocatable :: reversed
        integer(int32),   intent(in)  :: length
        integer(int32)                :: i, r

        allocate(character(len=length) :: reversed)
        do i = 1, length
            r = length - i + 1
            reversed(i:i) = string(r:r)
        end do
    end function strrev
end program q11068
