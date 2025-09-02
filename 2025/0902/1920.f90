program q1920
    use, intrinsic :: iso_fortran_env, only: int32, int64, INPUT_UNIT, OUTPUT_UNIT
    implicit none

    call program_main()

contains
    subroutine program_main()
        integer(int32)                :: n, m, i
        integer(int32),   allocatable :: arr(:), sort_buff(:), target(:)

        read(INPUT_UNIT, *) n
        allocate(arr(n), sort_buff(n))

        read(INPUT_UNIT, *) arr ! 공백 기준으로 배열에 입력

        call merge_sort(arr, 1, n, sort_buff)
        deallocate(sort_buff)

        read(INPUT_UNIT, *) m
        allocate(target(m))

        read(INPUT_UNIT, *) target

        do i = 1, m
            if (bin_search(arr, target(i), 1, n) > 0) then
                write(OUTPUT_UNIT, '(A)') "1"
            else
                write(OUTPUT_UNIT, '(A)') "0"
            end if
        end do
        deallocate(arr, target)
    end subroutine program_main

    recursive function bin_search(arr, target, low, high) result(num_index)
        integer(int32), intent(in) :: arr(:)
        integer(int32), intent(in) :: target, low, high
        integer(int32)             :: mid, mid_val, num_index

        if (low > high) then
            num_index = -1
            return
        end if

        mid = (low + high) / 2_int32
        if (arr(mid) == target) then
            num_index = mid
        else if (arr(mid) < target) then
            num_index = bin_search(arr, target, mid + 1_int32, high)
        else
            num_index = bin_search(arr, target, low, mid - 1_int32)
        end if
    end function bin_search

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
end program q1920
