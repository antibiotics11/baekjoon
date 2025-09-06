program q9996
    use, intrinsic :: iso_fortran_env, &
        only: int8, int64, INPUT_UNIT, OUTPUT_UNIT
    implicit none

    call program_main()

contains
    subroutine program_main()
        character(len=100)   :: pattern, subject
        integer(int8)        :: n, i
        logical, allocatable :: subject_matches(:)

        read(INPUT_UNIT, *    ) n
        read(INPUT_UNIT, "(A)") pattern

        allocate(subject_matches(n))
        do i = 1_int8, n, 1_int8
            subject = ""
            read(INPUT_UNIT, "(A)") subject
            subject_matches(i) = matches(pattern, subject)
        end do

        do i = 1_int8, n, 1_int8
            if (subject_matches(i)) then
                write(OUTPUT_UNIT, "('DA')")
            else
                write(OUTPUT_UNIT, "('NE')")
            end if
        end do
    end subroutine program_main

    logical function matches(pattern, subject)
        intrinsic :: len_trim
        character(len=*), intent(in) :: pattern,   &
                                        subject
        integer(int64)               :: p_ptr,     &
                                        s_ptr,     &
                                        star_idx,  &
                                        match_idx, &
                                        p_len,     &
                                        s_len

        matches   = .false.
        p_len     = len_trim(pattern)
        s_len     = len_trim(subject)
        p_ptr     = 1_int64
        s_ptr     = 1_int64
        star_idx  = 0_int64
        match_idx = 1_int64

        do while (s_ptr <= s_len)
            if (p_ptr <= p_len .and. pattern(p_ptr:p_ptr) == subject(s_ptr:s_ptr)) then
                p_ptr = p_ptr + 1
                s_ptr = s_ptr + 1
            else if (p_ptr <= p_len .and. pattern(p_ptr:p_ptr) == "*") then
                star_idx  = p_ptr
                match_idx = s_ptr
                p_ptr     = p_ptr + 1
            else if (star_idx > 0) then
                p_ptr     = star_idx + 1
                match_idx = match_idx + 1
                s_ptr     = match_idx
            else
                return
            end if
        end do
        
        do while (p_ptr < p_len .and. pattern(p_ptr:p_ptr) == "*")
            p_ptr = p_ptr + 1
        end do

        matches = (p_ptr > p_len)
    end function matches
end program q9996
