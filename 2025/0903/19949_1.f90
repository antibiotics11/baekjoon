program q19949
    use, intrinsic :: iso_fortran_env, only: INPUT_UNIT, OUTPUT_UNIT
    implicit none
    call program_main()

contains
    subroutine program_main()
        integer(kind=4) :: cnt
        integer(kind=1) :: answer_case(10), answer_key(10)

        read(INPUT_UNIT, *) answer_key

        cnt = 0_4
        call search(1_1, 0_1, 0_1, answer_case, answer_key, 0_1, cnt)

        write(OUTPUT_UNIT,'(I0)') cnt
    end subroutine program_main
    
    recursive subroutine search(q, prev_1, prev_2, answer_case, answer_key, score, cnt)
        integer(kind=1), intent(in)    :: q, prev_1, prev_2, score
        integer(kind=4), intent(inout) :: cnt
        integer(kind=1), intent(inout) :: answer_case(:)
        integer(kind=1), intent(in)    :: answer_key(:)
        integer(kind=1)                :: a, new_score

        do a = 1, 5
            if (prev_1 == a .and. prev_2 == a) then
                cycle ! 세문제 연속으로 답이 같으면 건너뛰기
            end if

            answer_case(q) = a

            new_score = score
            if (a == answer_key(q)) then ! 정답 맞췄는지 확인
                new_score = score + 1_1
            end if

            if (q == size(answer_case)) then
                if (new_score >= 5) then ! 점수 5점 이상이면 카운트
                    cnt = cnt + 1
                end if
            else
                call search(q + 1_1, a, prev_1, answer_case, answer_key, new_score, cnt)
            end if
        end do
    end subroutine search
end program q19949
