module integer_stack
    implicit none
    integer, parameter :: int64 = selected_int_kind(18)

    type :: stack
        integer(int64), allocatable :: items(:)
        integer(int64)              :: idx = 0
    end type stack

    contains    
        function stack_push(num, target) result(idx)
            integer(int64), intent(in)    :: num
            type(stack),    intent(inout) :: target
            integer(int64)                :: idx

            target%idx = target%idx + 1
            target%items(target%idx) = num
            idx = target%idx
        end function stack_push

        function stack_pop(target) result(top)
            type(stack), intent(inout) :: target
            integer(int64)             :: top
            
            if (target%idx == 0) then
                top = -1
                return
            end if
            
            top = target%items(target%idx)
            target%idx = target%idx - 1
            ! 남는 값은 굳이 지우지 않고 그냥 나중에 덮어쓰기
        end function stack_pop

        function stack_peek(target) result(top)
            type(stack), intent(inout) :: target
            integer(int64)             :: top
            
            if (target%idx == 0) then
                top = -1
                return
            end if
            
            top = target%items(target%idx)
        end function stack_peek

        function stack_size(target)
            type(stack), intent(inout) :: target
            integer(int64)             :: stack_size
            
            stack_size = target%idx
        end function stack_size

        function stack_is_empty(target)
            type(stack), intent(inout) :: target
            integer(int64)             :: stack_is_empty
            
            stack_is_empty = 1
            if (target%idx > 0) then
                stack_is_empty = 0
            end if
        end function stack_is_empty
end module integer_stack

program q10828
    use integer_stack
    character(len=4), parameter :: command_push  = "push"
    character(len=3), parameter :: command_pop   = "pop"
    character(len=3), parameter :: command_peek  = "top"
    character(len=4), parameter :: command_size  = "size"
    character(len=5), parameter :: command_empty = "empty"

    call program_main()

    contains
        subroutine program_main()
            character(len=15)         :: input_buffer
            character(:), allocatable :: input_tokens(:)
            character(len=5)          :: command
            integer(int64)            :: command_cnt, command_value, dummy
            type(stack)               :: main_stack

            read *, command_cnt

            allocate(main_stack%items(100000))
            do while (command_cnt > 0)
                command       = ""
                command_value = -1

                read(*, '(A)') input_buffer

                input_tokens    = explode(" ", input_buffer, 2_int64)
                command         = adjustl(input_tokens(1))
                input_tokens(2) = adjustl(input_tokens(2))
                !write(*, '(A)') command
                !write(*, '(A)') input_tokens(2)

                if (len_trim(input_tokens(2)) /= 0) then
                    read(input_tokens(2), *) command_value
                end if
                
                if (len_trim(input_tokens(2)) /= 0) then
                    dummy = route(main_stack, command, command_value)
                else
                    write(*, '(I0)') route(main_stack, command, command_value)
                end if

                command_cnt = command_cnt - 1
            end do
        end subroutine program_main

        function route(target, command, command_value)
            type(stack),      intent(inout) :: target
            character(len=5), intent(in)    :: command
            integer(int64),   intent(in)    :: command_value
            integer(int64)                  :: route
        
            select case (command)
            case (command_push);  route = stack_push(command_value, target)
            case (command_pop);   route = stack_pop(target)
            case (command_peek);  route = stack_peek(target)
            case (command_size);  route = stack_size(target)
            case (command_empty); route = stack_is_empty(target)
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
end program q10828
