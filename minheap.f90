! Author: Stephen DeVoy

! About this program:

! I've written this program as an exercise in updating my FORTRAN
! programming skills. This is the second FORTRAN program I've written
! since the early 1980s (Numerical Computing Class). Since then,
! I've been programming primarily in C, C++, Java, C#, Lisp, Python,
! PHP, Delphi, JavaScript, TypeScript, and Kotlin. I decided to revive
! my FORTRAN skills in order to include FORTRAN in the list of
! programming skills I offer as a freelance developer.

! This file implements a min-heap. I intend to use this min-heap as
! a priority queue in my coming implementation of Dijkstra's
! algorithm.

! Fortran has many limitations. Unlike C, there is no analog to a
! pointer to a void. It would be impossible to create a heap in Fortran
! which could use nodes of arbitrary type. Moreover, Fortran does not
! allow arrays of pointers. To get around this limitations, my implementation
! uses an array of MinHeapElement. Each MinHeapElement contains only one
! field, a pointer to an instance of Heapable. Each instance of Heapable
! contains a key, named value, of type integer. It also has another field,
! payload, which is also an integer. The idea is that nodes will be kept in
! their own array and the payload field will index that array of nodes.
! This provides most of the same functionality that could be obtained
! from a pointer to a void.

! Since Fortran does not have templated types, a heap indexed by strings
! or a heap indexed by REAL numbers would have to be implemented separately,
! but the code should be mostly the same, execpt for the type of the value field
! of Heapable.

MODULE HeapModule
    IMPLICIT NONE

    ! Our heaps will begin with enough space for 100 elements.
    ! Whenever the number of elements grows beyond this size,
    ! the array of elements will be reallocated with an additional
    ! 100 elements, allowing it to grow indefinitely.
    INTEGER, PARAMETER :: DEFAULT_HEAP_SIZE = 100

    ! This represents the content of a heap element.
    TYPE Heapable
        INTEGER :: value    ! The used in ordering comparisons
        INTEGER :: payload  ! Either an integer used as the payload
                            ! or an index into an array of elements
                            ! used to represent the objects managed
                            ! by the heap
    END TYPE Heapable

    ! This exists to compensate for Fortran's lack of pointer arrays.
    TYPE MinHeapElement
        TYPE(Heapable), POINTER :: ptr  ! The element pointed to at this index
    END TYPE MinHeapElement

    ! This represents the min-heap.
    TYPE MinHeap
        ! elements is a dynamic array of MinHeapElement
        TYPE(MinHeapElement), ALLOCATABLE :: elements(:)
        ! capacity is the number of element allocated for the elements array
        INTEGER :: capacity = DEFAULT_HEAP_SIZE
        ! the number of elements currently in use by the heap
        INTEGER :: size = 0
    END TYPE MinHeap

CONTAINS
    ! Creates a heap and initializes it.
    SUBROUTINE create_min_heap(heap)
        IMPLICIT NONE
        TYPE(MinHeap), INTENT(OUT) :: heap
        ALLOCATE(heap%elements(0:DEFAULT_HEAP_SIZE))
    END SUBROUTINE create_min_heap

    ! Swaps the pointers of two elements in the heap's array
    SUBROUTINE swap_heap_elements(e1, e2)
        IMPLICIT NONE
        TYPE(MinHeapElement), INTENT(INOUT) :: e1, e2
        BLOCK
            TYPE(Heapable), POINTER :: temp
            temp => e1%ptr
            e1%ptr => e2%ptr
            e2%ptr => temp
        END BLOCK
    END SUBROUTINE swap_heap_elements

    ! Reorders the heap
    RECURSIVE SUBROUTINE min_heapify(heap, index)
        IMPLICIT NONE
        TYPE(MinHeap), INTENT(INOUT) :: heap
        INTEGER, INTENT(IN) :: index
        BLOCK
            INTEGER :: smallest_index, left_index, right_index
            smallest_index = index
            left_index = 2 * index + 1
            right_index = 2 * index + 2
            IF (left_index < heap%size) THEN
                IF (heap%elements(left_index)%ptr%value < heap%elements(smallest_index)%ptr%value) THEN
                    smallest_index = left_index
                END IF
            END IF
            IF (right_index < heap%size) THEN
                IF (heap%elements(right_index)%ptr%value < heap%elements(smallest_index)%ptr%value) THEN
                    smallest_index = right_index
                END IF
            END IF
            IF (smallest_index /= index) THEN
                CALL swap_heap_elements(heap%elements(index), heap%elements(smallest_index))
                CALL min_heapify(heap, smallest_index)
            END IF
        END BLOCK
    END SUBROUTINE min_heapify

    ! Inserts a new element into the heap.
    SUBROUTINE insert_into_min_heap(heap, element)
        IMPLICIT NONE
        TYPE(MinHeap), INTENT(INOUT) :: heap
        TYPE(Heapable), TARGET, INTENT(INOUT) :: element
        BLOCK
            INTEGER :: i
            ! If the size of the heap equals the capacity of the heap,
            ! it is time to increase the capacity.
            IF (heap%size == heap%capacity) THEN
                BLOCK
                    TYPE(MinHeapElement), ALLOCATABLE :: temp(:)
                    ! Create a new array of greater size to hold the heap
                    heap%capacity = heap%capacity + DEFAULT_HEAP_SIZE
                    ALLOCATE(temp(heap%capacity))
                    ! Copy the elements from the old array into the new array
                    temp(1:SIZE(heap%elements)) = heap%elements
                    ! Swap the new array into elements
                    CALL MOVE_ALLOC(from=temp, to=heap%elements)
                END BLOCK
            END IF
            ! Increment the size of the heap and add the new element to the end.
            heap%size = heap%size + 1
            i = heap%size - 1
            heap%elements(i)%ptr => element
            ! Reorder the heap's elements
            DO WHILE (i /= 0)
                IF (.NOT. (heap%elements((i - 1) / 2)%ptr%value > heap%elements(i)%ptr%value)) THEN
                    EXIT
                END IF
                CALL swap_heap_elements(heap%elements(i), heap%elements((i - 1) / 2))
                i = (i - 1) / 2
            END DO
        END BLOCK
    END SUBROUTINE insert_into_min_heap

    ! The min element is at the top of the heap.
    ! Make element be that first element and
    ! reorder the heap to accomodate it's removal.
    ! Return .TRUE. if success and .FALSE. if failure.
    LOGICAL FUNCTION extract_min_from_min_heap(heap, element)
        IMPLICIT NONE
        TYPE(MinHeap), INTENT(INOUT) :: heap
        TYPE(Heapable), INTENT(OUT) :: element
        IF (heap%size <= 0) THEN
            extract_min_from_min_heap = .FALSE.
        ELSE IF (heap%size == 1) THEN
            heap%size = heap%size - 1
            element = heap%elements(0)%ptr
            extract_min_from_min_heap = .TRUE.
        ELSE
            element = heap%elements(0)%ptr
            heap%elements(0)%ptr => heap%elements(heap%size - 1)%ptr
            heap%size = heap%size - 1
            CALL min_heapify(heap, 0)
            extract_min_from_min_heap = .TRUE.
        END IF
    END FUNCTION extract_min_from_min_heap

    ! Print out the heap for debugging purposes.
    SUBROUTINE print_min_heap(heap)
        IMPLICIT NONE
        TYPE(MinHeap), INTENT(INOUT) :: heap
        BLOCK
            INTEGER :: i
            WRITE (*,*) "HEAP: "
            DO i = 0, (heap%size - 1)
                WRITE (*,*) heap%elements(i)%ptr%value
            END DO
        END BLOCK
    END SUBROUTINE print_min_heap
END MODULE HeapModule

PROGRAM main
    USE HeapModule
    IMPLICIT NONE
    BLOCK
        TYPE(MinHeap) :: heap
        TYPE(Heapable) :: e1, e2, e3, e4, e5, e6
        TYPE(Heapable) :: element

        CALL create_min_heap(heap)

        e1%value = 3;
        e2%value = 2;
        e3%value = 15;
        e4%value = 5;
        e5%value = 4;
        e6%value = 45;

        CALL insert_into_min_heap(heap, e1)
        CALL insert_into_min_heap(heap, e2)
        CALL insert_into_min_heap(heap, e3)
        CALL insert_into_min_heap(heap, e4)
        CALL insert_into_min_heap(heap, e5)
        CALL insert_into_min_heap(heap, e6)

        CALL print_min_heap(heap)

        IF (extract_min_from_min_heap(heap, element)) THEN
            WRITE (*,*) 'Extracted min: ', element%value
        ELSE
            WRITE (*,*) 'Failed to extract min'
        END IF

        CALL print_min_heap(heap)

        IF (extract_min_from_min_heap(heap, element)) THEN
            WRITE (*,*) 'Extracted min: ', element%value
        ELSE
            WRITE (*,*) 'Failed to extract min'
        END IF

        CALL print_min_heap(heap)
    END BLOCK
END PROGRAM main
