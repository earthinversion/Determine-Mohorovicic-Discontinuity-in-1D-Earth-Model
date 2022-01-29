! Obtain the Mohorovičić discontinuity node number in the 1D earth model files
program get_moho_node
    implicit none
    integer :: mohonode, totalnodes, totalDiscont, totalheaders=3
    ! character (len=256) :: model_file = 'model1D.dat' !SEMUCB
    character (len=256) :: model_file = 'model1D_prem.dat' !PREM

    call det_moho_node(model_file, totalheaders, totalnodes, totalDiscont, mohonode)

    write(*,21) 'Model file is : ', trim(model_file)
    write(*,22) 'Total #nodes is : ',totalnodes
    write(*,22) 'Total #discontinuities is : ',totalDiscont
    write(*,23) 'Moho node # is: ',mohonode,' (line #',mohonode+totalheaders,')'


    21 format(2A)
    22 format(A,I5)
    23 format(A,I5,A,I5,A)
    
end program get_moho_node


!! subroutine to get the moho node number
subroutine det_moho_node(model_file, totalheaders, num_lines, totalDiscont, mohonode)
    implicit none

    !! Declare vars
    integer :: FID = 10
    character (len=256), intent(in) :: model_file
    character (len=256) :: CTMP
    real (kind=8), allocatable :: radius(:),density(:),vpv(:),vsv(:),qkappa(:),qmu(:),vph(:),vsh(:),eta(:)
    real (kind=8), allocatable :: derivdensity(:), derivVp(:), derivVs(:)
    real (kind=8) :: tol = 2.0d0**(-5), maxderivdensity = 0.
    integer :: i = 0, IERR = 0
    integer, intent(in) :: totalheaders
    integer, intent(inout) :: num_lines, mohonode, totalDiscont
    integer :: j

    mohonode = 1 !initialize moho node
    num_lines = 0 !initialize num of nodes in the file
    open(FID, file=model_file, status="old",iostat=IERR)
    

    ! Get number of lines
    do i = 1, totalheaders
        read( FID, * )   !! skip the header
    end do
    
    do while (IERR == 0)
        num_lines = num_lines + 1
        read(FID,*,iostat=IERR) CTMP
    end do
    num_lines = num_lines - 1
    ! write(*,'(A,I0)') "Total number of nodes: ", num_lines


    ! Allocate array of strings
    allocate(radius(num_lines), density(num_lines), vpv(num_lines), vsv(num_lines), qkappa(num_lines))
    allocate(qmu(num_lines), vph(num_lines),vsh(num_lines),eta(num_lines))
    allocate(derivdensity(num_lines), derivVp(num_lines), derivVs(num_lines))
    
    

    ! Read the file content
    rewind(FID)
    do i = 1, totalheaders
        read( FID, * )   !! skip the header
    end do
    do i = 1, num_lines
        read(FID,*) radius(i), density(i), vpv(i), vsv(i), qkappa(i),qmu(i), vph(i),vsh(i),eta(i)  !Read the data
        ! print*, i, radius(i), density(i), vpv(i), vsv(i), qkappa(i),qmu(i), vph(i),vsh(i),eta(i) !! Print out the results    
    end do


    ! find the discontinuities
    totalDiscont = 0
    do i = 1, num_lines-1
        if (abs(radius(i+1)-radius(i)) < tol)  then
            derivdensity(i) = density(i+1)-density(i)
            derivVp(i) = vpv(i+1)-vpv(i)
            derivVs(i) = vsv(i+1)-vsv(i)
            ! write(*,11) i+3, i, derivdensity(i), radius(i), density(i), vpv(i), vsv(i), qkappa(i),qmu(i), vph(i), vsh(i),eta(i)
            
            totalDiscont = totalDiscont + 1
        else
            derivdensity(i) = (density(i+1)-density(i))/(radius(i+1)-radius(i))
            derivVp(i) = (vpv(i+1)-vpv(i))/(radius(i+1)-radius(i))
            derivVs(i) = (vsv(i+1)-vsv(i))/(radius(i+1)-radius(i))
        end if
    end do
    ! write(*,12) "Total discontinuities: ", totalDiscont

    
    
    
    ! Determine the Mohorovicic discontinuity node
    ! Conditions to select the moho discontinuity: 
    ! 1. Radius don't change, hence discontinuity
    ! 2. Vsv(i) and Vsv(i+1) > 0
    ! 3. delta Vsv > 0
    ! 4. max depth of 90km
    ! 5. max density change within 90 km from surface

    j = 1
    do i = 1, num_lines-1
        if (abs(radius(i+1)-radius(i)) < tol) then
            if ((abs(vsv(i)) > tol) .and. (abs(vsv(i+1)) > tol) .and. (abs(derivVs(i)) > tol) &
            .and. (abs(radius(i)-6371000.) < 90000.)) then
                ! write(*,13) i+3, i, radius(i), derivdensity(i), derivVp(i), derivVs(i)
                
                if (abs(derivdensity(i)) > maxderivdensity) then
                    maxderivdensity = abs(derivdensity(i))
                    mohonode = i
                end if
            end if
            j = j + 1
        end if
    end do
    
    
    
    ! write(*,12) 'Moho node number is: ',mohonode

    ! 11 format(2I5,10F12.2)
    ! 12 format(A,I5)
    ! 13 format(2I5,4F13.2)
    
    close(FID)
    deallocate(radius,density,vpv,vsv,qkappa,qmu,vph,vsh,eta)
    deallocate(derivdensity, derivVp, derivVs)


end subroutine det_moho_node