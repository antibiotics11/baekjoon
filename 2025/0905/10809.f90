program q10809
    use, intrinsic :: iso_fortran_env, &
        only: INPUT_UNIT, OUTPUT_UNIT
    implicit none
    intrinsic :: len_trim, ichar
    call program_main()

contains
    subroutine program_main()
        character(len=100) :: str
        integer(kind=8)    :: alphabets(97:122)
        integer(kind=8)    :: i, chr

        str = ""
        read(INPUT_UNIT, '(A)') str

        alphabets = -1_8
        do i = 1, len_trim(str)
            chr = ichar(str(i:i))
            if (alphabets(chr) == -1_8) then
                alphabets(chr) = i - 1
            end if
        end do

        do i = 97, 122
            write(OUTPUT_UNIT, '(I0, " ")', advance="no") alphabets(i)
        end do

        write(OUTPUT_UNIT, *)
    end subroutine program_main
end program q10809
