program q2775
    use, intrinsic :: iso_fortran_env, &
        only: input_unit, output_unit, int32
    implicit none

    call program_main()
contains
    subroutine program_main()
        integer(int32) :: n, k, t

        read(input_unit, *) t
        do while (t > 0)
            read(input_unit, *) k, n
            write(output_unit, "(I0)") residents(k, n)
            t = t - 1
        end do
    end subroutine program_main

    integer(int32) function residents(k, n)
        integer(int32), intent(in)  :: k, n
        integer(int32)              :: i, j
        integer(int32), allocatable :: dp(:,:)

        allocate(dp(0:k, 1:n))
        dp = 0
        forall (i = 1:n) dp(0, i) = i

        do i = 1, k
            do j = 1, n
                if (j == 1) then
                    dp(i, j) = 1
                    cycle
                end if
                dp(i, j) = dp(i, j - 1) + dp(i - 1, j)
            end do
        end do
        residents = dp(k, n)
    end function residents
end program q2775
