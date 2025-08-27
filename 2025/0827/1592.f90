module circular_linked_list
        implicit none
        integer, parameter :: int64 = selected_int_kind(18)

        type :: node
                integer(int64)      :: data
                type(node), pointer :: prev => null()
                type(node), pointer :: next => null()
        end type node

        type :: list
                type(node), pointer :: head => null()
        end type list

        contains
                subroutine insert_front(new_data, target_list)
                        integer(int64), intent(in)    :: new_data
                        type(list),     intent(inout) :: target_list
                        type(node),     pointer       :: new_node

                        allocate(new_node)
                        new_node%data = new_data

                        if (.not. associated(target_list%head)) then
                                target_list%head           => new_node
                                new_node%next              => new_node
                                new_node%prev              => new_node
                        else
                                new_node%next              => target_list%head
                                new_node%prev              => target_list%head%prev
                                target_list%head%prev%next => new_node
                                target_list%head%prev      => new_node
                                target_list%head           => new_node
                        end if
                end subroutine insert_front

end module circular_linked_list

program q1592
        use circular_linked_list
        integer(int64)      :: n, m, l, i, total = 0_int64
        type(list)          :: members
        type(node), pointer :: current_member

        read *, n, m, l
        do i = 1, n
                call insert_front(0_int64, members)
        end do

        current_member => members%head
        do while (.true.)
                current_member%data = current_member%data + 1_int64
                if (current_member%data == m) then
                        exit
                end if

                if (mod(current_member%data, 2_int64) == 0_int64) then
                        do i = 1, l
                                current_member => current_member%prev
                        end do
                else 
                        do i = 1, l
                                current_member => current_member%next
                        end do
                end if
                total = total + 1
        end do

        write(*, '(I0)') total
end program q1592
