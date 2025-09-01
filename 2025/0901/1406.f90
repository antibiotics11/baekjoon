module character_stack
    use iso_fortran_env
    implicit none

    type :: stack
        character(1), allocatable :: items(:)
        integer(int64)            :: idx = 0
    contains
        procedure :: construct   ! 생성자
        procedure :: push        ! 스택의 맨 위에 추가
        procedure :: pop         ! 스택의 맨 위를 제거
        procedure :: peek        ! 스택의 맨 위를 확인
        procedure :: is_empty    ! 스택이 비어있는지 확인
        procedure :: size        ! 스택의 현재 크기 확인
        procedure :: from_string ! 스택에 문자열을 입력
        procedure :: to_string   ! 스택을 문자열로 변환
    end type stack

contains
    subroutine construct(this, max_size)
        class(stack),   intent(inout) :: this
        integer(int64), intent(in)    :: max_size

        if (allocated(this%items)) then
            deallocate(this%items)
        end if
        allocate(this%items(max_size))
        this%idx   = 0_int64
        this%items = achar(0)
    end subroutine construct

    integer(int64) function push(this, item)
        class(stack), intent(inout) :: this
        character(1), intent(in)    :: item

        this%idx = this%idx + 1_int64
        this%items(this%idx) = item
        push = this%idx
    end function push

    character(1) function pop(this)
        class(stack), intent(inout) :: this

        pop = this%peek()   
        if (.not. this%is_empty()) then
            this%idx = this%idx - 1_int64
        end if
    end function pop

    character(1) function peek(this)
        class(stack), intent(in) :: this
        ! 비었으면 널바이트 반환
        peek = merge(achar(0), this%items(this%idx), this%is_empty())
    end function peek

    logical function is_empty(this)
        class(stack), intent(in) :: this
        is_empty = this%idx == 0_int64
    end function is_empty

    integer(int64) function size(this)
        class(stack), intent(in) :: this
        size = this%idx
    end function size

    subroutine from_string(this, input)
        class(stack), intent(inout) :: this
        character(*), intent(in)    :: input
        integer(int64)              :: idx, dummy

        do idx = 1_int64, len(input)
            dummy = this%push(input(idx:idx))
        end do
    end subroutine from_string

    function to_string(this, reverse) result(output)
        class(stack), intent(in)  :: this
        logical,      intent(in)  :: reverse
        character(:), allocatable :: output
        integer(int64)            :: idx

        allocate(character(this%idx) :: output)
        if (reverse) then
            do idx = this%idx, 1, -1
                output(this%idx - idx + 1:this%idx - idx + 1) = this%items(idx)
            end do
        else
            do idx = 1, this%idx
                output(idx:idx) = this%items(idx)
            end do
        end if
    end function to_string
end module character_stack

program q1406
    use iso_fortran_env
    use character_stack
    implicit none

    integer(int64), parameter :: stack_max_size = 600000
    character(1),   parameter :: command_cursor_inc = "L"
    character(1),   parameter :: command_cursor_dec = "D"
    character(1),   parameter :: command_stack_push = "P"
    character(1),   parameter :: command_stack_pop  = "B"

    call program_main()

contains
    subroutine program_main()
        character(:), allocatable :: input_buff
        character(:), allocatable :: input_tokens(:)
        character(1)              :: command, command_val
        integer(int64)            :: command_cnt
        type(stack)               :: left, right

        call left%construct(stack_max_size)
        call right%construct(stack_max_size)

        allocate(character(stack_max_size) :: input_buff)
        read(INPUT_UNIT, '(A)') input_buff
        read(INPUT_UNIT, *    ) command_cnt
        call left%from_string(trim(input_buff))
        deallocate(input_buff)

        allocate(character(3) :: input_buff)
        do while (command_cnt > 0)
            command     = achar(0)
            command_val = achar(0)

            read(INPUT_UNIT, '(A)') input_buff
            input_tokens = explode(" ", input_buff, 2_int64)
            command      = adjustl(input_tokens(1))
            command_val  = adjustl(input_tokens(2))

            call controller(left, right, command, command_val)

            command_cnt = command_cnt - 1_int64
        end do
        deallocate(input_buff)

        write(OUTPUT_UNIT, '(A, A)') left%to_string(.false.), right%to_string(.true.)

    end subroutine program_main

    subroutine controller(left, right, command, command_val)
        type(stack),  intent(inout) :: left, right
        character(1), intent(in)    :: command,command_val
        character(1)                :: chr_dummy
        integer(int64)              :: int_dummy

        select case (command)
        case (command_cursor_inc)            ! 커서를 왼쪽으로 이동
            if (.not. left%is_empty()) then
                int_dummy = right%push(left%pop())
            end if 

        case (command_cursor_dec)            ! 커서를 오른쪽으로 이동
            if (.not. right%is_empty()) then
                int_dummy = left%push(right%pop())
            end if

        case (command_stack_pop)             ! 커서 왼쪽 문자를 삭제
            chr_dummy = left%pop()

        case (command_stack_push)            ! 커서 왼쪽에 문자 추가
            int_dummy = left%push(command_val)

        end select
    end subroutine controller

    function explode(separator, string, limit) result(tokens)
        character(*),   intent(in)  :: separator
        character(*),   intent(in)  :: string
        integer(int64), intent(in)  :: limit
        integer(int64)              :: idx, pos
        character(:),   allocatable :: tmp
        character(:),   allocatable :: tokens(:)

        tmp = trim(adjustl(string))
        allocate(character(len=len(tmp)) :: tokens(limit))
        tokens = ""

        do idx = 1, limit
            pos = index(tmp, separator)

            if (pos == 0) then
                tokens(idx) = tmp
                exit
            else
                tokens(idx) = tmp(1:pos-1)
                tmp = tmp(pos+len(separator):)
            end if
        end do
    end function explode
end program q1406
