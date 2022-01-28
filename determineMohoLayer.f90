! Obtain the Mohorovičić discontinuity layer number in the 1D earth model files
program get_moho_layer
    implicit none
    integer :: moholayer, totallayers, totalDiscont, totalheaders=3
    ! character (len=256) :: model_file = 'model1D.dat' !SEMUCB
    character (len=256) :: model_file = 'model1D_prem.dat' !PREM

    call det_moho_layer(model_file, totalheaders, totallayers, totalDiscont, moholayer)

    write(*,21) 'Model file is : ', trim(model_file)
    write(*,22) 'Total #layers is : ',totallayers
    write(*,22) 'Total #discontinuities is : ',totalDiscont
    write(*,23) 'Moho layer # is: ',moholayer,' (line #',moholayer+totalheaders,')'


    21 format(2A)
    22 format(A,I5)
    23 format(A,I5,A,I5,A)
    
end program get_moho_layer


!! subroutine to get the moho layer number
subroutine det_moho_layer(model_file, totalheaders, NUM_LINES, totalDiscont, moholayer)
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
    integer, intent(inout) :: NUM_LINES, moholayer, totalDiscont
    integer :: j

    moholayer = 1 !initialize moho layer
    NUM_LINES = 0 !initialize num of layers in the file
    open(FID, file=model_file, status="old",iostat=IERR)
    

    ! Get number of lines
    do i = 1, totalheaders
        read( FID, * )   !! skip the header
    end do
    
    do while (IERR == 0)
        NUM_LINES = NUM_LINES + 1
        read(FID,*,iostat=IERR) CTMP
    end do
    NUM_LINES = NUM_LINES - 1
    ! write(*,'(A,I0)') "Total number of layers: ", NUM_LINES


    ! Allocate array of strings
    allocate(radius(NUM_LINES), density(NUM_LINES), vpv(NUM_LINES), vsv(NUM_LINES), qkappa(NUM_LINES))
    allocate(qmu(NUM_LINES), vph(NUM_LINES),vsh(NUM_LINES),eta(NUM_LINES))
    allocate(derivdensity(NUM_LINES), derivVp(NUM_LINES), derivVs(NUM_LINES))
    
    

    ! Read the file content
    rewind(FID)
    do i = 1, totalheaders
        read( FID, * )   !! skip the header
    end do
    do i = 1, NUM_LINES
        read(FID,*) radius(i), density(i), vpv(i), vsv(i), qkappa(i),qmu(i), vph(i),vsh(i),eta(i)  !Read the data
        ! print*, i, radius(i), density(i), vpv(i), vsv(i), qkappa(i),qmu(i), vph(i),vsh(i),eta(i) !! Print out the results    
    end do


    ! find the discontinuities
    totalDiscont = 0
    do i = 1, NUM_LINES-1
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

    
    
    
    ! Determine the Mohorovicic discontinuity layer
    ! Conditions to select the moho discontinuity: 
    ! 1. Radius don't change, hence discontinuity
    ! 2. Vsv(i) and Vsv(i+1) > 0
    ! 3. delta Vsv > 0
    ! 4. max depth of 90km
    ! 5. max density change within 90 km from surface

    j = 1
    do i = 1, NUM_LINES-1
        if (abs(radius(i+1)-radius(i)) < tol) then
            if ((abs(vsv(i)) > tol) .and. (abs(vsv(i+1)) > tol) .and. (abs(derivVs(i)) > tol) &
            .and. (abs(radius(i)-6371000.) < 90000.)) then
                ! write(*,13) i+3, i, radius(i), derivdensity(i), derivVp(i), derivVs(i)
                
                if (abs(derivdensity(i)) > maxderivdensity) then
                    maxderivdensity = abs(derivdensity(i))
                    moholayer = i
                end if
            end if
            j = j + 1
        end if
    end do
    
    
    
    ! write(*,12) 'Moho layer number is: ',moholayer

    ! 11 format(2I5,10F12.2)
    ! 12 format(A,I5)
    ! 13 format(2I5,4F13.2)
    
    close(FID)
    deallocate(radius,density,vpv,vsv,qkappa,qmu,vph,vsh,eta)
    deallocate(derivdensity, derivVp, derivVs)


end subroutine det_moho_layer