program q1463
    use, intrinsic :: iso_fortran_env, &
        only: int32, input_unit, output_unit
    implicit none

    call program_main()

contains
    subroutine program_main()
        intrinsic :: mod, min
        integer(int32), allocatable :: cache(:)
        integer(int32)              :: n, i, tmp

        read(input_unit, *) n

        allocate(cache(n))
        cache = -1
        tmp = 0

        cache(1) = 0
        cache(2) = 1
        do i = 3, n
            tmp = cache(i - 1) + 1
            if (mod(i, 2) == 0) then
                tmp = min(tmp, cache(i / 2) + 1)
            end if
            if (mod(i, 3) == 0) then
                tmp = min(tmp, cache(i / 3) + 1)
            end if
            cache(i) = tmp
        end do

        write(output_unit, "(I0)") cache(n)
    end subroutine program_main
end program q1463
