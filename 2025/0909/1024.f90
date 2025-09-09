program q1024
    use, intrinsic :: iso_fortran_env, &
        only: int8, int32, input_unit, output_unit
    implicit none

    call program_main()

contains
    subroutine program_main()
        logical        :: ap_exists
        integer(int32) :: s, &       ! 수열의 합  (s <= 1,000,000,000)
                          a, &       ! 첫째 항
                          l, &       ! 마지막 항
                          i, &
                          tmp
        integer(int8)  :: n          ! 항의 개수 (2 <= n <= 100)

        read(input_unit, *) s, n

        ap_exists = .false.
        do while (n <= 100 .and. .not. ap_exists)
            a = (2 * s / n - (n - 1)) / 2
            l = a + (n - 1)

            tmp = 0
            do i = a, l
                tmp = tmp + i
            end do

            if (tmp == s) then
                ap_exists = .true.
            end if

            n = n + 1
        end do

        if (a < 0 .or. .not. ap_exists) then
            write(output_unit, "('-1')")
            stop
        end if

        do i = a, l
            write(output_unit, "(I0, ' ')", advance="no") i
        end do
        write(output_unit, *)
    end subroutine program_main
end program q1024
