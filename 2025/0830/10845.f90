module integer_queue
    implicit none
    integer, parameter :: int64 = selected_int_kind(18)

    type :: queue
        integer(int64), allocatable :: items(:)
        integer(int64)              :: idx = 0_int64
        contains
            procedure :: enqueue
            procedure :: dequeue
            procedure :: size
            procedure :: is_empty
            procedure :: front
            procedure :: rear
    end type queue

contains
    integer(int64) function enqueue(this, num)
        class(queue),   intent(inout) :: this
        integer(int64), intent(in)    :: num

        this%idx = this%idx + 1_int64
        this%items(this%idx) = num
        enqueue = this%idx
    end function enqueue

    integer(int64) function dequeue(this)
        class(queue),   intent(inout) :: this
        integer(int64)                :: idx

        dequeue = this%front()
        if (dequeue == -1_int64) then
            return
        end if

        this%idx = this%idx - 1_int64
        do idx = 1_int64, this%idx
            this%items(idx) = this%items(idx + 1_int64)
        end do
        ! 마지막 인덱스는 굳이 지우지 않고 나중에 덮어쓰기
    end function dequeue

    integer(int64) function size(this)
        class(queue), intent(inout) :: this
        size = this%idx
    end function size

    integer(int64) function is_empty(this)
        class(queue), intent(inout) :: this
        is_empty = merge(0_int64, 1_int64, this%idx > 0_int64)
    end function is_empty

    integer(int64) function front(this)
        class(queue), intent(inout) :: this
        front = merge(-1_int64, this%items(1), this%is_empty() == 1_int64)
    end function front

    integer(int64) function rear(this)
        class(queue), intent(inout) :: this
        rear = merge(-1_int64, this%items(this%idx), this%is_empty() == 1_int64)
    end function rear
end module integer_queue

program q10845
    use integer_queue
    character(len=4), parameter :: command_enqueue  = "push"
    character(len=3), parameter :: command_dequeue  = "pop"
    character(len=4), parameter :: command_size     = "size"
    character(len=5), parameter :: command_is_empty = "empty"
    character(len=5), parameter :: command_front    = "front"
    character(len=4), parameter :: command_rear     = "back"

    call program_main()

contains
    subroutine program_main()
        character(len=15)         :: input_buffer
        character(:), allocatable :: input_tokens(:)
        character(len=5)          :: command
        integer(int64)            :: command_cnt, command_value, dummy
        type(queue)               :: q

       read *, command_cnt

        allocate(q%items(100000))
        do while (command_cnt > 0)
            command       = ""
            command_value = -1_int64

            read(*, '(A)') input_buffer

            input_tokens    = explode(" ", input_buffer, 2_int64)
            command         = adjustl(input_tokens(1))
            input_tokens(2) = adjustl(input_tokens(2))

            if (len_trim(input_tokens(2)) /= 0) then
                read(input_tokens(2), *) command_value
                dummy = route(q, command, command_value)
            else
                write(*, '(I0)') route(q, command, command_value)
            end if

            command_cnt = command_cnt - 1
        end do
    end subroutine program_main

    integer(int64) function route(target, command, command_value)
        type(queue),      intent(inout) :: target
        character(len=5), intent(in)    :: command
        integer(int64),   intent(in)    :: command_value
        
        select case (command)
        case (command_enqueue);  route = target%enqueue(command_value)
        case (command_dequeue);  route = target%dequeue()
        case (command_size);     route = target%size()
        case (command_is_empty); route = target%is_empty()
        case (command_front);    route = target%front()
        case (command_rear);     route = target%rear()
        end select
    end function route

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
end program q10845
