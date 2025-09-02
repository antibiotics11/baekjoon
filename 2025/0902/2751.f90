program q2751
    use, intrinsic :: iso_fortran_env, only: int32, INPUT_UNIT, OUTPUT_UNIT
    implicit none
    call program_main()

contains
    subroutine program_main()
        integer(int32)              :: n, i
        integer(int32), allocatable :: arr(:), sorted_arr(:)

        read(INPUT_UNIT, *) n
        allocate(arr(n), sorted_arr(n))

        do i = 1, n
            read(INPUT_UNIT, *) arr(i)
        end do
        call merge_sort(arr, 1, n, sorted_arr)

        do i = 1, n
            write(OUTPUT_UNIT, '(I0)') arr(i)
        end do
    end subroutine program_main

    recursive subroutine merge_sort(arr, low, high, sorted_arr)
        integer(int32), intent(inout) :: arr(:)
        integer(int32), intent(inout) :: sorted_arr(:)
        integer(int32), intent(in)    :: low, high
        integer(int32)                :: mid

        if (low < high) then
            mid = (low + high) / 2_int32
            call merge_sort(arr, low,           mid,  sorted_arr)
            call merge_sort(arr, mid + 1_int32, high, sorted_arr)
            call merge(arr, low, mid, high, sorted_arr)
        end if
    end subroutine merge_sort

    subroutine merge(arr, low, mid, high, sorted_arr)
        integer(int32), intent(inout) :: arr(:)
        integer(int32), intent(inout) :: sorted_arr(:)
        integer(int32), intent(in)    :: low, mid, high
        integer(int32)                :: left_i, right_i, sorted_i, i

        left_i   = low
        right_i  = mid + 1_int32
        sorted_i = low

        do while (left_i <= mid .and. right_i <= high)
            if (arr(left_i) <= arr(right_i)) then
                sorted_arr(sorted_i) = arr(left_i)
                left_i = left_i + 1_int32
            else
                sorted_arr(sorted_i) = arr(right_i)
                right_i = right_i + 1_int32
            end if
            sorted_i = sorted_i + 1_int32
        end do

        if (left_i > mid) then
            do i = right_i, high
                sorted_arr(sorted_i) = arr(i)
                sorted_i = sorted_i + 1_int32
            end do
        else
            do i = left_i, mid
                sorted_arr(sorted_i) = arr(i)
                sorted_i = sorted_i + 1_int32
            end do
        end if

        do i = low, high
            arr(i) = sorted_arr(i)
        end do
    end subroutine merge
end program q2751
