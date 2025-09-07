program q1463
    use, intrinsic :: iso_fortran_env, &
        only: int32, input_unit, output_unit
    implicit none

    call program_main()

contains
    subroutine program_main()
        integer(int32), allocatable :: cache(:)
        integer(int32)              :: n, i

        read(input_unit, *) n

        allocate(cache(n))
        cache = -1

        write(output_unit, "(I0)") min_cnt(n, cache)
    end subroutine program_main

    recursive function min_cnt(n, cache) result(cnt)
        intrinsic :: mod, min
        integer(int32), intent(inout) :: cache(:)
        integer(int32), intent(in)    :: n
        integer(int32)                :: cnt, nm1, nm2, nm3

        if (n <= 1) then
            cnt = 0
            return
        end if

        if (cache(n) /= -1) then
            cnt = cache(n)
            return
        end if

        nm1 = min_cnt(n - 1, cache) + 1
        nm2 = n
        nm3 = n
        if (mod(n, 2) == 0) then
            nm2 = min_cnt(n / 2, cache) + 1
        end if
        if (mod(n, 3) == 0) then
            nm3 = min_cnt(n / 3, cache) + 1
        end if

        cnt = min(nm1, nm2, nm3)
        cache(n) = cnt
    end function min_cnt
end program q1463
