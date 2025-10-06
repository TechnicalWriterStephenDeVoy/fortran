! Author: Stephen DeVoy

! About this program:

! I've written this program as an exercise in updating my FORTRAN
! programming skills. This is the first FORTRAN program I've written
! since the early 1980s (Numerical Computing Class). Since then,
! I've been programming primarily in C, C++, Java, C#, Lisp, Python,
! PHP, Delphi, JavaScript, TypeScript, and Kotlin. I decided to revive
! my FORTRAN skills in order to include FORTRAN in the list of
! programming skills I offer as a freelance developer.

! The modules at the beginning of this file will be broken out into
! separate files. For now, they are all included in one file to make
! it easier to compile for anyone that wishes to review my code.

! This program reads in a list of edges between cities. Each edge consists
! of an Origin, Destination, and Cost. The input file is a CSV file.
! Each line consists of a triple:

! origin,destin,cost

! The file is read twice. In the first pass, all city names are gathered
! and assigned a unique index (an integer). In the second pass, a matrix
! is created representing the triples. At the intersection of any two
! indexes, the cost of the edge (if there is an edge) connecting them is stored.

! The program will find all of the paths between any two cities, as well as the
! aggregated cost of the path.

! This program is a stepping stone towards my next self-assigned FORTRAN
! project. The next project will use the graph representation of this
! project as part of an implementation of Dijkstra's algorithm to find
! the lowest cost path between any two cities. The search mechanism for
! the second project will utilizie a balanced tree heap.

! To compile the program using gfortran on Linux:

! gfortran read_graph.f90 -o read_graph

! To run the program:

! ./read_graph graph2.dat

MODULE ListModule
    ! A module of linked list implementions.
    ! Currently, it only includes singly linked integer lists
    ! and lists of singly linked integer lists. For each type
    ! of list, there is an enumerator type.

    !!! FIRST ORDER LISTS
    TYPE IntegerListElem
        INTEGER :: value;
        TYPE(IntegerListElem), POINTER :: next
    END TYPE IntegerListElem

    TYPE IntegerList
        TYPE(IntegerListElem), POINTER :: head
    END TYPE IntegerList

    TYPE IntegerListIterator
        TYPE(IntegerListElem), POINTER :: current
    END TYPE IntegerListIterator

    !!! SECOND ORDER LISTS
    TYPE IntegerListListElem
        TYPE(IntegerList), POINTER :: value;
        TYPE(IntegerListListElem), POINTER :: next
    END TYPE

    TYPE IntegerListList
        TYPE(IntegerListListElem), POINTER :: head
    END TYPE IntegerListList

    TYPE IntegerListListIterator
        TYPE(IntegerListListElem), POINTER :: current
    END TYPE IntegerListListIterator

CONTAINS

    !!! FIRST ORDER LISTS

    SUBROUTINE print_integer_list(list)
        IMPLICIT NONE
        TYPE(IntegerList), POINTER, INTENT(INOUT) :: list
        BLOCK
            TYPE(IntegerListElem), POINTER :: current
            LOGICAL :: first_pass
            current => list%head
            first_pass = .TRUE.
            WRITE(*, '(A)', advance='no') '('
            DO WHILE (ASSOCIATED(current))
                IF (.NOT. first_pass) THEN
                    WRITE(*, '(A)', advance='no') ', '
                END IF
                WRITE(*, '(I0)', advance='no') current%value
                current => current%next
                first_pass = .FALSE.
            END DO
            WRITE(*, '(A)', advance='no') ')'
        END BLOCK
    END SUBROUTINE

    FUNCTION new_integer_list() RESULT(ptr)
        IMPLICIT NONE
        TYPE(IntegerList), POINTER :: ptr
        ALLOCATE(ptr)
        NULLIFY(ptr%head)
    END FUNCTION new_integer_list

    SUBROUTINE init_integer_list(list)
        IMPLICIT NONE
        TYPE(IntegerList), INTENT(INOUT) :: list
        NULLIFY(list%head)
    END SUBROUTINE init_integer_list

    INTEGER FUNCTION integer_list_length(list)
        IMPLICIT NONE
        TYPE(IntegerList), INTENT(INOUT) :: list
        IF (.NOT. ASSOCIATED(list%head)) THEN
            integer_list_length = 0
        ELSE
            BLOCK
                INTEGER :: count
                TYPE(IntegerListElem), POINTER :: current
                count = 0
                current => list%head
                DO WHILE (ASSOCIATED(current))
                    count = count + 1
                    current => current%next
                END DO
            integer_list_length = count
            END BLOCK
        END IF
    END FUNCTION integer_list_length

    FUNCTION integer_list_to_array(list) RESULT(arr)
        IMPLICIT NONE
        TYPE(IntegerList), INTENT(INOUT) :: list
        INTEGER, ALLOCATABLE, DIMENSION(:) :: arr
        BLOCK
            INTEGER :: size, i
            TYPE(IntegerListElem), POINTER :: current
            i = 1
            size = integer_list_length(list)
            ALLOCATE(arr(size))
            current => list%head
            DO WHILE (ASSOCIATED(current))
                arr(i) = current%value
                current => current%next;
                i = i + 1
            END DO
        END BLOCK
    END FUNCTION

    SUBROUTINE clear_integer_list(list)
        IMPLICIT NONE
        TYPE(IntegerList), INTENT(INOUT) :: list
        IF (ASSOCIATED(list%head)) THEN
            BLOCK
                TYPE(IntegerListElem), POINTER :: current
                TYPE(IntegerListElem), POINTER :: next
                current => list%head
                DO WHILE (ASSOCIATED(current))
                    next => current%next;
                    DEALLOCATE(current)
                    current => next
                END DO
            END BLOCK
        ENDIF
    END SUBROUTINE clear_integer_list

    SUBROUTINE reverse_integer_list(list)
        IMPLICIT NONE
        TYPE(IntegerList), INTENT(INOUT) :: list
        IF (ASSOCIATED(list%head)) THEN
            BLOCK
                TYPE(IntegerListElem), POINTER :: previous
                TYPE(IntegerListElem), POINTER :: current
                TYPE(IntegerListElem), POINTER :: next
                NULLIFY(previous)
                current => list%head
                NULLIFY(next)
                DO WHILE (ASSOCIATED(current))
                    next => current%next
                    current%next => previous
                    previous => current
                    current => next
                END DO
                list%head => previous
            END BLOCK
        END IF
    END SUBROUTINE

    SUBROUTINE free_integer_list(list)
        IMPLICIT NONE
        TYPE(IntegerList), POINTER, INTENT(INOUT) :: list
        IF (ASSOCIATED(list)) THEN
            CALL clear_integer_list(list)
            DEALLOCATE(list)
        END IF
    END SUBROUTINE

    FUNCTION copy_integer_list(list) RESULT(cpy)
        IMPLICIT NONE
        TYPE(IntegerList), POINTER, INTENT(INOUT) :: list
        TYPE(IntegerList), POINTER :: cpy
        cpy => new_integer_list()
        IF (ASSOCIATED(list%head)) THEN
            BLOCK
                TYPE(IntegerListElem), POINTER :: orig_current
                TYPE(IntegerListElem), POINTER :: new_current

                WRITE (*,*) "Allocated"

                orig_current => list%head
                ALLOCATE(new_current)
                new_current%value = orig_current%value
                cpy%head => new_current

                WRITE (*,*) "About to enter loop"

                DO WHILE (ASSOCIATED(orig_current%next))
                    ALLOCATE(new_current%next)
                    new_current%next%value = orig_current%next%value
                    orig_current => orig_current%next
                    new_current => new_current%next
                 END DO
            END BLOCK
        END IF
    END FUNCTION

    SUBROUTINE push_onto_integer_list(list, value)
        IMPLICIT NONE
        TYPE(IntegerList), POINTER, INTENT(INOUT) :: list
        INTEGER, INTENT(IN) :: value
        BLOCK
            TYPE(IntegerListElem), POINTER :: new_elem;
            ALLOCATE(new_elem)
            new_elem%value = value;
            new_elem%next => list%head
            list%head => new_elem
        END BLOCK
    END SUBROUTINE push_onto_integer_list

    INTEGER FUNCTION pop_off_of_integer_list(list, val_if_empty)
        IMPLICIT NONE
        TYPE(IntegerList), POINTER, INTENT(INOUT) :: list
        INTEGER, INTENT(IN) :: val_if_empty
        IF (.NOT. ASSOCIATED(list%head)) THEN
            pop_off_of_integer_list = val_if_empty
        ELSE
            pop_off_of_integer_list = list%head%value
            BLOCK
                TYPE(IntegerListElem), POINTER :: temp
                temp => list%head
                list%head => temp%next
                DEALLOCATE(temp)
            END BLOCK
        END IF
    END FUNCTION pop_off_of_integer_list

    SUBROUTINE add_to_end_of_integer_list(list, value)
        IMPLICIT NONE
        TYPE(IntegerList), POINTER, INTENT(INOUT) :: list
        INTEGER, INTENT(IN) :: value
        !WRITE (*, *) "add_to_end_of_integer_list:", value
        BLOCK
            TYPE(IntegerListElem), POINTER :: current
            TYPE(IntegerListElem), POINTER :: new_elem
            ALLOCATE(new_elem)
            new_elem%value = value
            NULLIFY(new_elem%next)
            !WRITE (*, *) " created new element", value
            IF (.NOT. ASSOCIATED(list%head)) THEN
                list%head => new_elem
            ELSE IF (.NOT. ASSOCIATED(list%head%next)) THEN
                list%head%next => new_elem
            ELSE
                current => list%head
                DO WHILE (ASSOCIATED(current%next))
                    current => current%next
                END DO
                current%next => new_elem
            END IF
        END BLOCK
    END SUBROUTINE add_to_end_of_integer_list

    INTEGER FUNCTION first_element_of_integer_list(list, val_if_empty)
        IMPLICIT NONE
        TYPE(IntegerList), POINTER, INTENT(INOUT) :: list
        INTEGER, INTENT(IN) :: val_if_empty
        
        IF (.NOT. ASSOCIATED(list%head)) THEN
            first_element_of_integer_list = val_if_empty
        ELSE
            first_element_of_integer_list = list%head%value
        END IF
    END FUNCTION first_element_of_integer_list

    SUBROUTINE init_integer_list_iterator(list, iterator)
        IMPLICIT NONE
        TYPE(IntegerList), POINTER, INTENT(INOUT) :: list
        TYPE(IntegerListIterator), INTENT(INOUT) :: iterator
        iterator%current => list%head
    END SUBROUTINE init_integer_list_iterator

    LOGICAL FUNCTION integer_list_next(iterator, value)
        IMPLICIT NONE
        TYPE(IntegerListIterator), INTENT(INOUT) :: iterator
        INTEGER, INTENT(OUT) :: value
        IF (.NOT. ASSOCIATED(iterator%current)) THEN
            integer_list_next = .FALSE. 
        ELSE
            value = iterator%current%value
            iterator%current => iterator%current%next
            integer_list_next = .TRUE.
        END IF
    END FUNCTION integer_list_next

    !!! SECOND ORDER LISTS

    SUBROUTINE print_integer_list_list(list)
        IMPLICIT NONE
        TYPE(IntegerListList), POINTER, INTENT(INOUT) :: list
        BLOCK
            TYPE(IntegerListListElem), POINTER :: current
            current => list%head
            DO WHILE (ASSOCIATED(current))
                CALL print_integer_list(current%value)
                WRITE(*, '(A)') ''
                current => current%next
            END DO
        END BLOCK
    END SUBROUTINE

    FUNCTION new_integer_list_list() RESULT(ptr)
        IMPLICIT NONE
        TYPE(IntegerListList), POINTER :: ptr
        ALLOCATE(ptr)
        NULLIFY(ptr%head)
    END FUNCTION new_integer_list_list

    SUBROUTINE init_integer_list_list(list)
        IMPLICIT NONE
        TYPE(IntegerListList), INTENT(INOUT) :: list
        NULLIFY(list%head)
    END SUBROUTINE init_integer_list_list

    INTEGER FUNCTION integer_list_list_length(list)
        IMPLICIT NONE
        TYPE(IntegerListList), INTENT(INOUT) :: list
        IF (.NOT. ASSOCIATED(list%head)) THEN
            integer_list_list_length = 0
        ELSE
            BLOCK
                INTEGER :: count
                TYPE(IntegerListListElem), POINTER :: current
                count = 0
                current => list%head
                DO WHILE (ASSOCIATED(current))
                    count = count + 1
                    current => current%next
                END DO
            integer_list_list_length = count
            END BLOCK
        END IF
    END FUNCTION integer_list_list_length

    FUNCTION integer_list_list_to_array(list) RESULT(arr)
        IMPLICIT NONE
        TYPE(IntegerListList), INTENT(INOUT) :: list
        TYPE(IntegerList), ALLOCATABLE, DIMENSION(:) :: arr
        BLOCK
            INTEGER :: size, i
            TYPE(IntegerListListElem), POINTER :: current
            i = 1
            size = integer_list_list_length(list)
            ALLOCATE(arr(size))
            current => list%head
            DO WHILE (ASSOCIATED(current))
                arr(i) = current%value
                current => current%next;
                i = i + 1
            END DO
        END BLOCK
    END FUNCTION integer_list_list_to_array

    SUBROUTINE clear_integer_list_list(list)
        IMPLICIT NONE
        TYPE(IntegerListList), INTENT(INOUT) :: list
        IF (ASSOCIATED(list%head)) THEN
            BLOCK
                TYPE(IntegerListListElem), POINTER :: current
                TYPE(IntegerListListElem), POINTER :: next
                current => list%head
                DO WHILE (ASSOCIATED(current))
                    next => current%next;
                    DEALLOCATE(current)
                    current => next
                END DO
            END BLOCK
        ENDIF
    END SUBROUTINE clear_integer_list_list

    SUBROUTINE reverse_integer_list_list(list)
        IMPLICIT NONE
        TYPE(IntegerListList), INTENT(INOUT) :: list
        IF (ASSOCIATED(list%head)) THEN
            BLOCK
                TYPE(IntegerListListElem), POINTER :: previous
                TYPE(IntegerListListElem), POINTER :: current
                TYPE(IntegerListListElem), POINTER :: next
                NULLIFY(previous)
                current => list%head
                NULLIFY(next)
                DO WHILE (ASSOCIATED(current))
                    next => current%next
                    current%next => previous
                    previous => current
                    current => next
                END DO
                list%head => previous
            END BLOCK
        END IF
    END SUBROUTINE

    SUBROUTINE free_integer_list_list(list)
        IMPLICIT NONE
        TYPE(IntegerListList), POINTER, INTENT(INOUT) :: list
        IF (ASSOCIATED(list)) THEN
            CALL clear_integer_list_list(list)
            DEALLOCATE(list)
        END IF
    END SUBROUTINE

    FUNCTION shallow_copy_integer_list_list(list) RESULT(cpy)
        IMPLICIT NONE
        TYPE(IntegerListList), POINTER, INTENT(INOUT) :: list
        TYPE(IntegerListList), POINTER :: cpy
        cpy => new_integer_list_list()
        IF (ASSOCIATED(list%head)) THEN
            BLOCK
                TYPE(IntegerListListElem), POINTER :: orig_current
                TYPE(IntegerListListElem), POINTER :: new_current

                WRITE (*,*) "Allocated"

                orig_current => list%head
                ALLOCATE(new_current)
                new_current%value => orig_current%value
                cpy%head => new_current

                WRITE (*,*) "About to enter loop"

                DO WHILE (ASSOCIATED(orig_current%next))
                    ALLOCATE(new_current%next)
                    new_current%next%value => orig_current%next%value
                    orig_current => orig_current%next
                    new_current => new_current%next
                 END DO
            END BLOCK
        END IF
    END FUNCTION

    SUBROUTINE push_onto_integer_list_list(list, value)
        IMPLICIT NONE
        TYPE(IntegerListList), POINTER, INTENT(INOUT) :: list
        TYPE(IntegerList), POINTER, INTENT(INOUT) :: value
        BLOCK
            TYPE(IntegerListListElem), POINTER :: new_elem;
            ALLOCATE(new_elem)
            new_elem%value => value;
            new_elem%next => list%head
            list%head => new_elem
        END BLOCK
    END SUBROUTINE push_onto_integer_list_list

    SUBROUTINE push_onto_all_list_list(list, value)
        IMPLICIT NONE
        TYPE(IntegerListList), POINTER, INTENT(INOUT) :: list
        INTEGER, INTENT(IN) :: value
        BLOCK
            TYPE(IntegerListListElem), POINTER :: current;
            IF (ASSOCIATED(list%head)) THEN
                current => list%head
                DO WHILE (ASSOCIATED(current))
                    CALL push_onto_integer_list(current%value, value)
                    current => current%next;
                END DO
            END IF
        END BLOCK
    END SUBROUTINE

    FUNCTION pop_off_of_integer_list_list(list) RESULT(res)
        IMPLICIT NONE
        TYPE(IntegerListList), POINTER, INTENT(INOUT) :: list
        TYPE(IntegerList), POINTER :: res

        IF (.NOT. ASSOCIATED(list%head)) THEN
            NULLIFY(res)
        ELSE
            res => list%head%value
            BLOCK
                TYPE(IntegerListListElem), POINTER :: temp
                temp => list%head
                list%head => temp%next
                DEALLOCATE(temp)
            END BLOCK
        END IF
    END FUNCTION pop_off_of_integer_list_list

    SUBROUTINE add_to_end_of_integer_list_list(list, value)
        IMPLICIT NONE
        TYPE(IntegerListList), POINTER, INTENT(INOUT) :: list
        Type(IntegerList), POINTER, INTENT(IN) :: value
        BLOCK
            TYPE(IntegerListListElem), POINTER :: current
            TYPE(IntegerListListElem), POINTER :: new_elem
            ALLOCATE(new_elem)
            new_elem%value => value
            NULLIFY(new_elem%next)
            IF (.NOT. ASSOCIATED(list%head)) THEN
                list%head => new_elem
            ELSE IF (.NOT. ASSOCIATED(list%head%next)) THEN
                list%head%next => new_elem
            ELSE
                current => list%head
                DO WHILE (ASSOCIATED(current%next))
                    current => current%next
                END DO
                current%next => new_elem
            END IF
        END BLOCK
    END SUBROUTINE add_to_end_of_integer_list_list

    FUNCTION first_element_of_integer_list_list(list) RESULT(res)
        IMPLICIT NONE
        TYPE(IntegerListList), POINTER, INTENT(INOUT) :: list
        TYPE(IntegerList), POINTER :: res
        
        IF (.NOT. ASSOCIATED(list%head)) THEN
            NULLIFY(res)
        ELSE
            res => list%head%value
        END IF
    END FUNCTION first_element_of_integer_list_list

    SUBROUTINE init_integer_list_list_iterator(list, iterator)
        IMPLICIT NONE
        TYPE(IntegerListList), POINTER, INTENT(INOUT) :: list
        TYPE(IntegerListListIterator), INTENT(INOUT) :: iterator
        iterator%current => list%head
    END SUBROUTINE init_integer_list_list_iterator

    LOGICAL FUNCTION integer_list_list_next(iterator, value)
        IMPLICIT NONE
        TYPE(IntegerListListIterator), INTENT(INOUT) :: iterator
        Type(IntegerList), POINTER, INTENT(OUT) :: value
        IF (.NOT. ASSOCIATED(iterator%current)) THEN
            integer_list_list_next = .FALSE. 
        ELSE
            value => iterator%current%value
            iterator%current => iterator%current%next
            integer_list_list_next = .TRUE.
        END IF
    END FUNCTION integer_list_list_next

    SUBROUTINE append_integer_list_list(destin, source)
        IMPLICIT NONE
        TYPE(IntegerListList), POINTER, INTENT(INOUT) :: destin
        TYPE(IntegerListList), POINTER, INTENT(INOUT) :: source
        IF (.NOT. ASSOCIATED(destin%head)) THEN
            destin%head => source%head
        ELSE
            BLOCK
                TYPE(IntegerListListElem), POINTER :: destin_end
                destin_end => destin%head
                DO WHILE (ASSOCIATED(destin_end%next))
                    destin_end => destin_end%next
                END DO
                destin_end%next => source%head
            END BLOCK
        END IF
        NULLIFY(source%head)
    END SUBROUTINE append_integer_list_list
END MODULE

MODULE GraphDataTypes
    ! This module implements a graph.
    USE ListModule
    TYPE NamedEdgeRecord                ! Named edges are read in from the data file.
        CHARACTER(LEN=50) :: from_node
        CHARACTER(LEN=50) :: to_node
        INTEGER :: cost
    END TYPE NamedEdgeRecord
    TYPE GraphNode                      ! GraphNode represents a node in the graph (name and index).
        CHARACTER(LEN=50) :: name
        INTEGER :: index
    END TYPE GraphNode
    INTEGER, PARAMETER :: default_dynamic_array_size = 10
    TYPE DynamicGraphNodeArray          ! Array of GraphNode. Dynamic in that it may grow in size.
        INTEGER :: available = default_dynamic_array_size
        INTEGER :: len = 0
        TYPE(GraphNode), ALLOCATABLE :: nodes(:)
    END TYPE DynamicGraphNodeArray
    TYPE EdgeMatrix                     ! EdgeMatrix hold a node count and a matrix of integers.
        INTEGER :: node_count
        INTEGER, ALLOCATABLE :: edge_matrix(:, :)
    END TYPE
CONTAINS
    SUBROUTINE print_node(node)
        IMPLICIT NONE
        TYPE(GraphNode), INTENT(IN) :: node
        PRINT *, node%name, '(', node%index, ')'
    END SUBROUTINE
    SUBROUTINE print_dynamic_graph_node_array(dgna)
        IMPLICIT NONE
        TYPE(DynamicGraphNodeArray), INTENT(IN) :: dgna
        BLOCK
            INTEGER :: i 
            DO i = 1, dgna%len
                CALL print_node(dgna%nodes(i))
            END DO
        END BLOCK
    END SUBROUTINE
    SUBROUTINE initialize_dynamic_graph_node_array(dgna)
        IMPLICIT NONE
        TYPE(DynamicGraphNodeArray), INTENT(INOUT) :: dgna
        ALLOCATE(dgna%nodes(default_dynamic_array_size))
    END SUBROUTINE
    SUBROUTINE deallocate_dynamic_graph_node_array(dgna)
        IMPLICIT NONE
        TYPE(DynamicGraphNodeArray), INTENT(INOUT) :: dgna
        DEALLOCATE(dgna%nodes)
    END SUBROUTINE
    INTEGER FUNCTION is_node_name(dgna, name)
        IMPLICIT NONE
        TYPE(DynamicGraphNodeArray), INTENT(INOUT) :: dgna
        CHARACTER(LEN=50), INTENT(IN) :: name
        BLOCK
            INTEGER :: i, result
            result = 0
            DO i = 1, dgna%len
                IF (dgna%nodes(i)%name == name) THEN
                    result = i
                    EXIT
                END IF
            END DO
            is_node_name = result
        END BLOCK
    END FUNCTION
    CHARACTER(LEN=50) FUNCTION name_given_node(dgna, node)
        IMPLICIT NONE
        TYPE(DynamicGraphNodeArray), INTENT(INOUT) :: dgna
        INTEGER, INTENT(IN) :: node
        name_given_node = dgna%nodes(node)%name
    END FUNCTION name_given_node
    SUBROUTINE add_node_to_dgnai_if_new(dgna, name)
        IMPLICIT NONE
        TYPE(DynamicGraphNodeArray), INTENT(INOUT) :: dgna
        CHARACTER(LEN=50), INTENT(IN) :: name
        IF (is_node_name(dgna, name) == 0) THEN
            IF (dgna%len == dgna%available) THEN
                BLOCK
                    TYPE(GraphNode), ALLOCATABLE :: temp(:)
                    dgna%available = dgna%available + default_dynamic_array_size
                    ALLOCATE(temp(dgna%available))
                    temp(1:size(dgna%nodes)) = dgna%nodes
                    CALL MOVE_ALLOC(from=temp, to=dgna%nodes)
                END BLOCK
            END IF
            
            dgna%len = dgna%len + 1
            dgna%nodes(dgna%len)%name = name
            dgna%nodes(dgna%len)%index = dgna%len
        END IF
    END SUBROUTINE
    SUBROUTINE deallocate_edge_matrix(em)
        IMPLICIT NONE
        TYPE(EdgeMatrix), INTENT(INOUT) :: em
        DEALLOCATE(em%edge_matrix)
    END SUBROUTINE
    SUBROUTINE get_edges_from(em, v, ea, c)
        IMPLICIT NONE
        TYPE(EdgeMatrix), INTENT(INOUT) :: em
        INTEGER, INTENT(IN) :: v
        !INTEGER, ALLOCATABLE, INTENT(INOUT) :: ea(:)
        INTEGER, INTENT(INOUT) :: ea(:)
        INTEGER, INTENT(OUT) :: c
        BLOCK
            INTEGER :: ea_len, max_len, i, j
            ea_len = SIZE(ea)
            max_len = MIN(ea_len, em%node_count)
            j = 0
            DO i = 1, max_len
                IF (em%edge_matrix(v, i) /= 0) THEN
                    j = j + 1
                    ea(j) = i
                END IF
            END DO
            c = j
        END BLOCK
    END SUBROUTINE
    INTEGER FUNCTION get_edge_cost(em, v1, v2)
        TYPE(EdgeMatrix), INTENT(INOUT) :: em
        INTEGER, INTENT(IN) :: v1, v2
        get_edge_cost = em%edge_matrix(v1, v2)
    END FUNCTION
    SUBROUTINE initialize_edge_matrix(em, dgna, named_edges)
        IMPLICIT NONE
        TYPE(EdgeMatrix), INTENT(INOUT) :: em
        TYPE(DynamicGraphNodeArray), INTENT(INOUT) :: dgna
        TYPE(NamedEdgeRecord), ALLOCATABLE, dimension(:), INTENT(INOUT) :: named_edges
        BLOCK
            INTEGER :: node_count, edge_count, i, from_node_index, to_node_index, cost
            CHARACTER(LEN=50) :: from_node_name, to_node_name
            node_count = dgna%len
            edge_count = SIZE(named_edges)
            ALLOCATE(em%edge_matrix(node_count, node_count))
            em%node_count = node_count
            em%edge_matrix = 0

            !WRITE (*,*) "edge_count is", edge_count

            DO i = 1, edge_count
                from_node_name = named_edges(i)%from_node
                WRITE (*,*) "from_node_name is", from_node_name
                to_node_name = named_edges(i)%to_node
                WRITE (*,*) "to_node_name is", to_node_name
                cost = named_edges(i)%cost
                from_node_index = is_node_name(dgna, from_node_name)
                WRITE (*,*) "from_node_index is", from_node_index
                to_node_index = is_node_name(dgna, to_node_name)
                WRITE (*,*) "to_node_index is", to_node_index
                em%edge_matrix(from_node_index, to_node_index) = cost
            END DO
        END BLOCK
    END SUBROUTINE
    RECURSIVE SUBROUTINE find_all_path_aux(em, v1, v2, pths, visited)
        IMPLICIT NONE
        TYPE(EdgeMatrix), INTENT(INOUT) :: em
        INTEGER, INTENT(IN) :: v1
        INTEGER, INTENT(IN) :: v2
        TYPE(IntegerListList), POINTER, INTENT(INOUT) :: pths
        LOGICAL, DIMENSION(:), INTENT(INOUT) :: visited
        WRITE (*,*) 'Entered find_all_path_aux'
        IF (.NOT. visited(v1)) THEN
            visited(v1) = .TRUE.
            BLOCK
                INTEGER :: edge_verticies(1:em%node_count)
                INTEGER :: edge_count
                INTEGER :: i, v
                TYPE(IntegerListList), POINTER :: all_paths
                all_paths => new_integer_list_list()
                CALL get_edges_from(em, v1, edge_verticies, edge_count)
                IF (edge_count /= 0) THEN
                    DO i = 1, edge_count
                        v = edge_verticies(i)
                        IF (v == v2) THEN
                            !!! reached destination
                            BLOCK
                                TYPE(IntegerList), POINTER :: new_path
                                new_path => new_integer_list()
                                CALL add_to_end_of_integer_list(new_path, v)
                                CALL add_to_end_of_integer_list_list(all_paths, new_path)
                            END BLOCK
                        ELSE IF (.NOT. visited(v)) THEN
                            ! reached a node we haven't visited
                            BLOCK
                                TYPE(IntegerListList), POINTER :: returned_paths
                                CALL find_all_path_aux(em, v, v2, returned_paths, visited)
                                !IF (ASSOCIATED(returned_paths)) THEN
                                    CALL push_onto_all_list_list(returned_paths, v)
                                    CALL append_integer_list_list(all_paths, returned_paths)
                                    DEALLOCATE(returned_paths)
                                !END IF
                            END BLOCK
                        END IF
                    END DO
                END IF
                CALL print_integer_list_list(all_paths)
                pths => all_paths
            END BLOCK
        END IF
        WRITE (*,*) 'Exiting find_all_path_aux'
    END SUBROUTINE
    SUBROUTINE find_all_paths(em, v1, v2, pths)
        IMPLICIT NONE
        TYPE(EdgeMatrix), INTENT(INOUT) :: em
        INTEGER, INTENT(IN) :: v1
        INTEGER, INTENT(IN) :: v2
        TYPE(IntegerListList), POINTER, INTENT(OUT) :: pths
        WRITE (*,*) 'Entered find_all_paths'
        !NULLIFY(pths)
        BLOCK
            LOGICAL :: visited(em%node_count)
            visited = .FALSE.
            ! visited(v1) = .TRUE.
            CALL find_all_path_aux(em, v1, v2, pths, visited)
            IF (ASSOCIATED(pths)) THEN
                CALL push_onto_all_list_list(pths, v1)
            END IF
        END BLOCK
        WRITE (*,*) 'Exiting find_all_paths'
    END SUBROUTINE
    INTEGER FUNCTION count_records_in_csv_file(f)
        IMPLICIT NONE
        CHARACTER(LEN=256), INTENT(IN) :: f  ! Dummy argument for radius
        BLOCK
            INTEGER ierr, ios, count
            CHARACTER(LEN=256) :: line
            OPEN(UNIT=10, FILE=TRIM(f), STATUS='OLD', ACTION='READ', IOSTAT=ierr)
            IF (ierr /= 0) THEN
                count_records_in_csv_file = 0
                PRINT *, 'Error opening file!'
            ELSE
                count = 0
                READ(10, '(A)', IOSTAT=ios) line
                DO WHILE (ios == 0)
                    IF (INDEX(line(1:), ',') == 0) THEN
                        EXIT
                    ELSE
                        count = count + 1
                    END IF
                    READ(10, '(A)', IOSTAT=ios) line
                END DO
                CLOSE(10)
                count_records_in_csv_file = count
            END IF
        END BLOCK
    END FUNCTION count_records_in_csv_file
END MODULE GraphDataTypes

MODULE InputAndOutput
    ! Simple text menu based IO for this program.
    USE GraphDataTypes
CONTAINS
    INTEGER FUNCTION get_choice()
        IMPLICIT NONE
        BLOCK
            INTEGER:: choice
            PRINT *, "Menu Options:"
            PRINT *, "1. List Cities"
            PRINT *, "2. Enter an Origin City"
            PRINT *, "3. Enter an Destination City"
            PRINT *, "4. List All Paths"
            PRINT *, "5. Exit"
            PRINT *, "Enter your choice (1-5):"
            READ *, choice
            get_choice = choice
        END BLOCK
    END FUNCTION get_choice
    INTEGER FUNCTION get_city(dgna)
        IMPLICIT NONE
        TYPE(DynamicGraphNodeArray), INTENT(IN) :: dgna
        BLOCK
            INTEGER :: i, choice
            PRINT *, "Menu Options:"
            DO i = 1, dgna%len
                WRITE(*, '(I0)', advance='no') i
                WRITE(*, '(A)', advance='no') '. '
                WRITE(*, '(A)', advance='no') TRIM(dgna%nodes(i)%name)
                PRINT *, ''
            END DO
            WRITE(*, '(A)', advance='no') 'Enter your choice (1-'
            WRITE(*, '(I0)', advance='no') i
            WRITE(*, '(A)', advance='no') '):'
            PRINT *, ''
            READ *, choice
            get_city = choice
        END BLOCK
    END FUNCTION get_city
    SUBROUTINE print_path(em, dgna, path, print_distances)
        IMPLICIT NONE
        TYPE(EdgeMatrix), INTENT(INOUT) :: em
        TYPE(DynamicGraphNodeArray), INTENT(IN) :: dgna
        TYPE(IntegerList), POINTER, INTENT(INOUT) :: path
        LOGICAL, INTENT(IN) :: print_distances
        BLOCK
            TYPE(IntegerListIterator) :: enum
            INTEGER :: origin, destin, cost, total_cost
            origin = 0
            total_cost = 0
            CALL init_integer_list_iterator(path, enum)
            DO WHILE(integer_list_next(enum, destin))
                IF (origin /= 0) THEN
                    WRITE(*, '(A)', advance='no') TRIM(dgna%nodes(origin)%name)
                    IF (print_distances) THEN
                        cost = get_edge_cost(em, origin, destin)
                        total_cost = total_cost + cost
                        WRITE(*, '(A)', advance='no') ' ('
                        WRITE(*, '(I0)', advance='no') cost
                        WRITE(*, '(A)', advance='no') ') '
                    ELSE
                        WRITE(*, '(A)', advance='no') ' > '
                    END IF
                END IF
                origin = destin
            END DO
            WRITE(*, '(A)', advance='no') TRIM(dgna%nodes(destin)%name)
            IF (print_distances) THEN
                WRITE(*, '(A)', advance='no') ' : '
                WRITE(*, '(I0)', advance='no') total_cost
            END IF
        END BLOCK
    END SUBROUTINE print_path
    SUBROUTINE print_paths(em, dgna, paths, print_distances)
        IMPLICIT NONE
        TYPE(EdgeMatrix), INTENT(INOUT) :: em
        TYPE(DynamicGraphNodeArray), INTENT(IN) :: dgna
        TYPE(IntegerListList), POINTER, INTENT(INOUT) :: paths
        LOGICAL, INTENT(IN) :: print_distances
        BLOCK
            TYPE(IntegerListListIterator) :: enum
            TYPE(IntegerList), POINTER :: path
            CALL init_integer_list_list_iterator(paths, enum)
            DO WHILE (integer_list_list_next(enum, path))
                CALL print_path(em, dgna, path, print_distances)
                WRITE (*,*) ''
            END DO
        END BLOCK
    END SUBROUTINE print_paths
END MODULE InputAndOutput

PROGRAM main
    USE GraphDataTypes
    USE InputAndOutput
    IMPLICIT NONE
    TYPE(NamedEdgeRecord), ALLOCATABLE :: named_edges(:) ! Allocatable array
    TYPE(DynamicGraphNodeArray) :: nodes
    TYPE(EdgeMatrix) :: edge_matrix
    INTEGER :: i, num_args, ierr, ios, graph_edge_record_count
    CHARACTER(LEN=256) :: file_name
    CHARACTER(LEN=256) :: line
    CHARACTER(LEN=50) :: field_str
    CHARACTER(LEN=50) :: str_val
    INTEGER :: int_val
    REAL :: real_val
    INTEGER :: comma_pos, field_count, record_index
    TYPE(IntegerListList), POINTER :: all_paths

    !NULLIFY(all_paths)

    ! Get the number of command-line arguments
    num_args = COMMAND_ARGUMENT_COUNT()

    ! Get the first argument
    IF (num_args /= 1) THEN
        WRITE (*,*) 'Usage: read_csv <file name>'
    ELSE
        CALL GET_COMMAND_ARGUMENT(1, file_name)
        WRITE (*,*) 'File name: ', TRIM(file_name);

        ! Count the edges in the file.
        graph_edge_record_count = count_records_in_csv_file(TRIM(file_name))

        IF (graph_edge_record_count > 0) THEN
            WRITE (*,*) 'Record count: ', graph_edge_record_count;
        END IF

        OPEN(UNIT=10, FILE=TRIM(file_name), STATUS='OLD', ACTION='READ', IOSTAT=ierr)
        IF (ierr /= 0) THEN
            PRINT *, 'Error opening file!'
            STOP
        END IF
        CALL initialize_dynamic_graph_node_array(nodes)
        ALLOCATE(named_edges(graph_edge_record_count))
        ! initialize variables used in loop
        record_index = 1
        READ(10, '(A)', IOSTAT=ios) line
        ! While there are more lines to read
        DO WHILE (ios == 0)
            comma_pos = 0
            field_count = 0
            ! WRITE (*, '(A,I0)', advance='no') 'Read: '
            ! Process a line
            DO
                comma_pos = INDEX(line(1:), ',')
                IF (comma_pos == 0) THEN
                    ! If this is the last field in the line,
                    ! it is the cost.
                    field_str = line(1 : )
                    str_val = field_str
                    READ(str_val, *) named_edges(record_index)%cost
                    record_index = record_index + 1
                    ! WRITE (*,'(A,I0)') TRIM(ADJUSTL(str_val))
                    EXIT
                ELSE
                    ! If this is not the last field in the line,
                    ! it is an origin or destination.
                    field_str = line(1 : comma_pos - 1)
                    str_val = field_str
                    if (field_count == 0) THEN
                        ! process origin
                        named_edges(record_index)%from_node = str_val;
                    ELSE
                        ! process destiniation
                        named_edges(record_index)%to_node = str_val;
                    END IF
                    ! add the node to our array of nodes
                    CALL add_node_to_dgnai_if_new(nodes, str_val)
                    ! WRITE (*, '(A,I0)', advance='no') TRIM(ADJUSTL(str_val))
                    ! WRITE (*, '(A,I0)', advance='no') ','
                END IF
                line = line(comma_pos + 1:)
                field_count = field_count + 1
            END DO
            READ(10, '(A)', IOSTAT=ios) line
        END DO
        ! Initialize the edge matrix with costs
        CALL initialize_edge_matrix(edge_matrix, nodes, named_edges)
        CALL print_dynamic_graph_node_array(nodes)
        ! Handle IO and execute user choices
        BLOCK
            INTEGER :: menu_choice, origin_node, destin_node
            CHARACTER(LEN=50) :: origin_city, destin_city
            ! Initialize variables used in loop
            origin_node = 0
            destin_node = 0
            origin_city = 'None'
            destin_city = 'None'
            menu_choice = get_choice()
            DO
                ! Processes the user's menu choice
                SELECT CASE (menu_choice)
                    CASE (1)
                        ! List nodes
                        CALL print_dynamic_graph_node_array(nodes)
                    CASE (2)
                        ! Set origin node
                        origin_node = get_city(nodes)
                        origin_city = name_given_node(nodes, origin_node)
                    CASE (3)
                        ! Set destination node
                        destin_node = get_city(nodes)
                        destin_city = name_given_node(nodes, destin_node)
                    CASE (4)
                        ! Compute all paths
                        IF (origin_node /= 0) THEN
                            IF (destin_node /= 0) THEN
                                BLOCK
                                    PRINT *, ""
                                    PRINT *, "Finding all paths..."
                                    CALL find_all_paths(edge_matrix, origin_node, destin_node, all_paths)
                                    PRINT *, ""
                                    PRINT *, "Paths:"
                                    IF (ASSOCIATED(all_paths)) THEN
                                        ! CALL print_integer_list_list(all_paths)
                                        CALL print_paths(edge_matrix, nodes, all_paths, .TRUE.)
                                    ELSE
                                        PRINT *, "No paths found."
                                    END IF
                                    PRINT *, ""
                                END BLOCK
                            ELSE
                                PRINT *, "Select a destination node before finding all paths."
                            END IF
                        ELSE
                            PRINT *, "Select n origin node before finding all paths."
                        END IF
                    CASE (5)
                        PRINT *, "Exiting program."
                        EXIT
                    CASE DEFAULT
                        PRINT *, "Invalid selection. Please try again."
                END SELECT
                WRITE(*, '(A)', advance='no') 'Origin City = '
                WRITE(*, '(A)', advance='no') TRIM(origin_city)
                WRITE(*, '(A)', advance='no') ', Destination City = '
                WRITE(*, '(A)', advance='no') TRIM(destin_city)
                PRINT *, ""
                menu_choice = get_choice()
            END DO
        END BLOCK

        CALL deallocate_dynamic_graph_node_array(nodes)
        DEALLOCATE(named_edges)
        CALL deallocate_edge_matrix(edge_matrix)
        CLOSE(10)
    END IF

END PROGRAM main
