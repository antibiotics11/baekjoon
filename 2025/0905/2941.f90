program q2941
    use, intrinsic :: iso_fortran_env, &
        only: INPUT_UNIT, OUTPUT_UNIT
    implicit none
    intrinsic :: len_trim, trim

    call program_main()

contains
    subroutine program_main()
        character(len=102), allocatable :: str
        integer(kind=8)               :: cnt, i, n

        str = ""
        read(INPUT_UNIT, "(A)") str

        n   = len_trim(str)
        i   = 1
        cnt = 0
        do while (i <= n)
            if (is_croatian_alp(str(i:i+1))) then
                i = i + 2
            else if (is_croatian_alp(str(i:i+2))) then
                i = i + 3
            else
                i = i + 1
            end if
            cnt = cnt + 1
        end do

        write(OUTPUT_UNIT, "(I0)") cnt
    end subroutine program_main

    logical function is_croatian_alp(chr)
        character(len=*), intent(in) :: chr

        select case (trim(chr))
        case ( &
            "c=",  &
            "c-",  &
            "dz=", &
            "d-",  &
            "lj",  &
            "nj",  &
            "s=",  &
            "z="   &
        );  is_croatian_alp = .true.
        case default
            is_croatian_alp = .false.
        end select
    end function is_croatian_alp
end program q2941
