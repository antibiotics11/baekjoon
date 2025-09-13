program q1919
    use, intrinsic :: iso_fortran_env, &
        only: input_unit, output_unit, int16
    implicit none
    intrinsic :: len_trim, max

    call program_main()

contains
    subroutine program_main()
        character(len=1000) :: word1, word2

        read(input_unit, "(A)") word1
        read(input_unit, "(A)") word2
        write(output_unit, "(I0)") minchr(word1, word2)
    end subroutine program_main

    integer(int16) function minchr(word1, word2)
        character(len=*), intent(in)  :: word1, word2
        character(len=:), allocatable :: buff1, buff2
        integer(int16)                :: i, pos

        allocate(character(len=len_trim(word1)) :: buff1)
        allocate(character(len=len_trim(word2)) :: buff2)
        buff1 = word1
        buff2 = word2

        minchr = 0
        do i = 1, len_trim(buff1)
            pos = chrpos(buff2, buff1(i:i))
            if (pos /= -1 .and. buff2(pos:pos) /= "Z") then
                buff1(i:i) = "Z"
                buff2(pos:pos) = "Z"
                cycle
            end if
            minchr = minchr + 1
        end do
        do i = 1, len_trim(buff2)
            if (buff2(i:i) == "Z") then
                cycle
            end if
            pos = chrpos(buff1, buff2(i:i))
            if (pos /= -1 .and. buff1(pos:pos) /= "Z") then
                buff1(pos:pos) = "Z"
                cycle
            end if
            minchr = minchr + 1
        end do
    end function minchr

    integer(int16) function chrpos(haystack, needle)
        character(len=*), intent(in) :: haystack
        character(len=1), intent(in) :: needle
        integer(int16)               :: i

        chrpos = -1
        do i = 1, len_trim(haystack)
            if (haystack(i:i) == needle) then
                chrpos = i
                return
            end if
        end do
    end function chrpos
end program q1919
