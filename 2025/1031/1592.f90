program q1592
    use, intrinsic :: iso_fortran_env, only: int32
    implicit none

    call program_main()
contains
    subroutine program_main()
        use, intrinsic :: iso_fortran_env, &
            only: input_unit, output_unit
        integer(int32)              :: n, m, l, total
        integer(int32)              :: pos
        integer(int32), allocatable :: member(:)

        read(input_unit, *) n, m, l

        allocate(member(n))
        member = 0
        pos    = 1
        total  = 0

        do while (.true.)
            member(pos) = member(pos) + 1
            if (member(pos) == m) then
                exit
            end if
            total = total + 1
            pos   = next_pos(n, l, pos, member(pos))
        end do
        write(output_unit, "(I0)") total
    end subroutine program_main

    integer(int32) function next_pos(n, l, pos, cnt)
        integer(int32), intent(in) :: n, l, pos, cnt
        next_pos = merge(        &
            mod(pos - l + n, n), &
            mod(pos + l    , n), &
            mod(cnt, 2) == 0     &
        )
        if (next_pos == 0) then
            next_pos = n
        end if
    end function next_pos
end program q1592
