program q1996
    use iso_fortran_env
    implicit none

    call program_main()

contains
    subroutine program_main()
        integer(int64)              :: row
        integer(int64)              :: map_size
        integer(int64), allocatable :: map_arr(:, :)
        character(:),   allocatable :: row_str

        read(INPUT_UNIT, *) map_size

        allocate(character(map_size) :: row_str)
        allocate(map_arr(map_size, map_size))

        do row = 1, map_size
            read(INPUT_UNIT, '(A)') row_str
            map_arr(row, :) = read_row(row_str, map_size)
        end do

        call fill_map(map_arr, map_size)
        write(OUTPUT_UNIT, '(A)') format_map(map_arr, map_size)
    end subroutine program_main

    function read_row(row_str, row_length) result(row)
        character(*),   intent(in)  :: row_str
        integer(int64), intent(in)  :: row_length
        integer(int64)              :: row_index
        integer(int64)              :: ascii_code
        integer(int64), allocatable :: row(:)

        allocate(row(row_length))
        do row_index = 1, row_length
            row(row_index) = 0

            ascii_code = ichar(row_str(row_index:row_index))
            if (ascii_code >= 48 .and. ascii_code <= 57) then
                row(row_index) = (ascii_code - 48) * (-1) ! 지뢰는 부호 반전시켜서 표시
            end if
        end do
    end function read_row
    
    subroutine fill_map(map_arr, map_size)
        integer(int64), intent(inout), target :: map_arr(:, :)
        integer(int64), intent(in)    :: map_size
        integer(int64)                :: row, column
        integer(int64), pointer       :: target
        integer(int64)                :: mine_cnt
        logical                       :: upper_row_exists, lower_row_exists
        logical                       :: upper_col_exists, lower_col_exists

        do row = 1, map_size
            do column = 1, map_size
                if (.not. is_mine(map_arr, row, column)) cycle

                mine_cnt = map_arr(row, column) * (-1)
                upper_row_exists = row < map_size     ! row + 1이 존재하는지
                lower_row_exists = row > 1            ! row - 1이 존재하는지
                upper_col_exists = column < map_size  ! col + 1이 존재하는지
                lower_col_exists = column > 1         ! col - 1이 존재하는지

                if (lower_row_exists) then
                    if (.not. is_mine(map_arr, row - 1, column)) then
                        target => map_arr(row - 1, column)
                        target =  target + mine_cnt
                    end if
                end if
                if (upper_row_exists) then
                    if (.not. is_mine(map_arr, row + 1, column)) then
                        target => map_arr(row + 1, column)
                        target =  target + mine_cnt
                    end if
                end if
                if (lower_col_exists) then
                    if (.not. is_mine(map_arr, row, column - 1)) then
                        target => map_arr(row, column - 1)
                        target = target + mine_cnt
                    end if
                end if
                if (upper_col_exists) then
                    if (.not. is_mine(map_arr, row, column + 1)) then
                        target => map_arr(row, column + 1)
                        target =  target + mine_cnt
                    end if
                end if
                if (lower_row_exists .and. lower_col_exists) then
                    if (.not. is_mine(map_arr, row - 1, column - 1)) then
                        target => map_arr(row - 1, column - 1);
                        target =  target + mine_cnt
                    end if
                end if
                if (upper_row_exists .and. upper_col_exists) then
                    if (.not. is_mine(map_arr, row + 1, column + 1)) then
                        target => map_arr(row + 1, column + 1)
                        target =  target + mine_cnt
                    end if
                end if
                if (lower_row_exists .and. upper_col_exists) then
                    if (.not. is_mine(map_arr, row - 1, column + 1)) then
                        target => map_arr(row - 1, column + 1)
                        target =  target + mine_cnt
                    end if
                end if
                if (upper_row_exists .and. lower_col_exists) then
                    if (.not. is_mine(map_arr, row + 1, column - 1)) then
                        target => map_arr(row + 1, column - 1)
                        target =  target + mine_cnt
                    end if
                end if
            end do
        end do
    end subroutine fill_map
    
    logical function is_mine(map_arr, row, column)
        integer(int64), intent(in) :: map_arr(:, :)
        integer(int64)             :: row, column
        is_mine = map_arr(row, column) < 0
    end function
    
    function format_map(map_arr, map_size) result(map_str)
        integer(int64)              :: row, column
        integer(int64)              :: str_length, str_index
        integer(int64), intent(in)  :: map_arr(:, :)
        integer(int64), intent(in)  :: map_size
        character(:),   allocatable :: map_str

        str_length = map_size * map_size + map_size - 1
        allocate(character(str_length) :: map_str)

        str_index = 1
        do row = 1, map_size
            do column = 1, map_size
                if (is_mine(map_arr, row, column)) then ! 부호 반전된 숫자(지뢰)인 경우
                    map_str(str_index:str_index) = '*'
                else if (map_arr(row, column) > 9) then ! 10 이상인 경우
                    map_str(str_index:str_index) = 'M'
                else
                    map_str(str_index:str_index) = achar(map_arr(row, column) + 48)
                end if

                str_index = str_index + 1
            end do

            if (row /= map_size) then
                map_str(str_index:str_index) = achar(10)
                str_index = str_index + 1
            end if
        end do
    end function format_map
end program q1996
