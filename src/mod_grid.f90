module mod_grid
use mod_constants, only: dp, tabCN
use mod_utility, only: lower_case
use mod_parameters, only: simulation, par_method
implicit none

! header of the grid file for integer matrix
type grid_header_i
    integer::jmax               ! n. of columns (ncols)
    integer::imax               ! n. of rows (nrows)
    real(dp)::x0                ! x low left corner xllcorner
    real(dp)::y0                ! y low left corner xllcorner
    character(len=55)::x0string
    character(len=55)::y0string
    real(dp)::cellsize          ! cell dimension in meter
    integer::nan                ! no data value
end type grid_header_i

! header of the grid file for real matrix
type grid_header_r
    integer::jmax               ! n. of columns (ncols)
    integer::imax               ! n. of rows (nrows)
    real(dp)::x0                ! x low left corner xllcorner
    real(dp)::y0                ! y low left corner xllcorner
    character(len=55)::x0string
    character(len=55)::y0string
    real(dp)::cellsize          ! cell dimension in meter
    real(dp)::nan               ! no data value
end type grid_header_r

! raster grid integer data
type grid_i
    type(grid_header_i)::header
    integer,dimension(:,:),pointer::mat
end type grid_i

! raster grid real data
type grid_r
    type(grid_header_r)::header
    real(dp),dimension(:,:),pointer::mat
end type grid_r

! extention
type bound
    type(grid_header_i)::header
    integer::i_north,i_south,j_west,j_est
end type bound

! overloading interface
interface read_grid
    module procedure read_grid_i, read_grid_r
end interface

interface write_grid
    module procedure write_grid_i, write_grid_r
end interface

interface check_header
    module procedure  check_header_i, check_header_r
end interface

interface overlay_domain
    module procedure  overlay_domain_i, overlay_domain_r
end interface

interface print_mat_as_grid
    module procedure write_header_and_mat_r, write_head_and_mat_i
end interface

interface set_default_par
    module procedure  set_default_par_i, set_default_par_r
end interface

! equal operator for grid object
interface assignment(=)
    module procedure eq_grid_rr,eq_grid_ri,eq_grid_ii,eq_grid_ir
end interface

interface id_to_par
    module procedure id_to_par_i, id_to_par_r
end interface

contains

subroutine eq_grid_ii(grid_out,grid_in)
    type(grid_i),intent(in)::grid_in
    type(grid_i),intent(out)::grid_out

    grid_out%header%jmax = grid_in%header%jmax
    grid_out%header%imax = grid_in%header%imax
    grid_out%header%x0 = grid_in%header%x0
    grid_out%header%x0string = grid_in%header%x0string
    grid_out%header%y0 = grid_in%header%y0
    grid_out%header%y0string = grid_in%header%y0string
    grid_out%header%cellsize = grid_in%header%cellsize
    grid_out%header%nan = grid_in%header%nan
    allocate(grid_out%mat(size(grid_in%mat,1),size(grid_in%mat,2)))
end subroutine eq_grid_ii

subroutine eq_grid_rr(grid_out,grid_in)
    type(grid_r),intent(in)::grid_in
    type(grid_r),intent(out)::grid_out

    grid_out%header%jmax = grid_in%header%jmax
    grid_out%header%imax = grid_in%header%imax
    grid_out%header%x0 = grid_in%header%x0
    grid_out%header%x0string = grid_in%header%x0string
    grid_out%header%y0 = grid_in%header%y0
    grid_out%header%y0string = grid_in%header%y0string
    grid_out%header%cellsize = grid_in%header%cellsize
    grid_out%header%nan = grid_in%header%nan
    allocate(grid_out%mat(size(grid_in%mat,1),size(grid_in%mat,2)))
end subroutine eq_grid_rr

subroutine eq_grid_ri(grid_out,grid_in)
    type(grid_i),intent(in)::grid_in
    type(grid_r),intent(out)::grid_out

    grid_out%header%jmax = grid_in%header%jmax
    grid_out%header%imax = grid_in%header%imax
    grid_out%header%x0 = grid_in%header%x0
    grid_out%header%x0string = grid_in%header%x0string
    grid_out%header%y0 = grid_in%header%y0
    grid_out%header%y0string = grid_in%header%y0string
    grid_out%header%cellsize = grid_in%header%cellsize
    grid_out%header%nan = real(grid_in%header%nan)
    allocate(grid_out%mat(size(grid_in%mat,1),size(grid_in%mat,2)))
end subroutine eq_grid_ri

subroutine eq_grid_ir(grid_out,grid_in)
    type(grid_r),intent(in)::grid_in
    type(grid_i),intent(out)::grid_out

    grid_out%header%jmax = grid_in%header%jmax
    grid_out%header%imax = grid_in%header%imax
    grid_out%header%x0 = grid_in%header%x0
    grid_out%header%x0string = grid_in%header%x0string
    grid_out%header%y0 = grid_in%header%y0
    grid_out%header%y0string = grid_in%header%y0string
    grid_out%header%cellsize = grid_in%header%cellsize
    grid_out%header%nan = int(grid_in%header%nan)
    allocate(grid_out%mat(size(grid_in%mat,1),size(grid_in%mat,2)))
end subroutine eq_grid_ir

subroutine read_grid_r( filename, prm, sim, extent)
    ! read ascii grid file *.ASC
    ! if present(sim)=.TRUE. ==> check georeference information with those stored in simulation parameters
    ! if present(extent)=.TRUE. ==> only value inside bound are loaded
   character(len=*), intent(in) :: filename
    type(grid_r),intent(out)::prm
    type(simulation),optional,intent(in)::sim
    type(bound),optional,intent(in)::extent

    integer :: i
    integer :: free_unit
    integer :: ios ! To check for read errors or end of file
    real(dp),dimension(:,:),allocatable::aux_parametro
    character(len=14)::str

    ios=0

    ! open the file in read only mode
    open(newunit=free_unit, file=trim(filename), status='old', action="read", iostat=ios)
    if (ios /= 0 ) then
        print *, "Cannot open file ", trim(filename), ". The specified file does not exist. Execution will be aborted..."
        stop
    end if
    if(.not.present(extent))then
        read(free_unit,*)str,prm%header%jmax
        read(free_unit,*)str,prm%header%imax
        read(free_unit,'(1a10,1f20.10)')str,prm%header%x0
        backspace(free_unit)
        read(free_unit,*)str,prm%header%x0string
        read(free_unit,'(1a10,1f20.10)')str,prm%header%y0
        backspace(free_unit)
        read(free_unit,*)str,prm%header%y0string
        read(free_unit,*)str,prm%header%cellsize
        read(free_unit,*)str,prm%header%nan
        if(present(sim))call check_header(sim,prm%header,filename)
        allocate(prm%mat(prm%header%imax,prm%header%jmax))
        do i=1,size(prm%mat,1)
            read(free_unit,*) prm%mat(i,:)
        end do
    else
        read(free_unit,*)str,prm%header%jmax
        read(free_unit,*)str,prm%header%imax
        read(free_unit,'(1a10,1f20.10)')str,prm%header%x0
        read(free_unit,'(1a10,1f20.10)')str,prm%header%y0
        read(free_unit,*)str,prm%header%cellsize
        read(free_unit,*)str,prm%header%nan
        if(present(sim))call check_header(sim,prm%header,filename)
        allocate(aux_parametro(prm%header%imax,prm%header%jmax))
        prm%header%jmax=extent%header%jmax
        prm%header%imax=extent%header%imax
        prm%header%x0=extent%header%x0
        prm%header%x0string=extent%header%x0string
        prm%header%y0=extent%header%y0
        prm%header%y0string=extent%header%y0string
        do i=1,size(aux_parametro,1)
            read(free_unit,*)aux_parametro(i,:)
        end do
        allocate(prm%mat(prm%header%imax,prm%header%jmax))
        prm%mat(1:size(prm%mat,1),1:size(prm%mat,2))= &
            aux_parametro(extent%i_north:extent%i_south,extent%j_west:extent%j_est)
        deallocate(aux_parametro)
    end if
    close(free_unit)

end subroutine read_grid_r

subroutine read_grid_i( filename, prm, sim, extent)
    ! read ascii grid file *.ASC
    ! if present(sim)=.TRUE. ==> check georeference information with those stored in simulation parameters
    ! if present(extent)=.TRUE. ==> only value inside bound are loaded
   character(len=*), intent(in) :: filename
    type(grid_i),intent(out)::prm
    type(simulation),optional,intent(in)::sim
    type(bound),optional,intent(in)::extent

    integer :: i
    integer :: free_unit
    integer :: ios              ! To check for read errors or end of file
    integer :: ierror           ! %AB% To check for read errors in file content (real instead of integer)
    integer,dimension(:,:),allocatable::aux_parametro
    character(len=14)::str

    ios=0
    ierror=0

    ! open the file in read only mode
    open(newunit=free_unit, file=trim(filename), status='old', action="read", iostat=ios)
    if (ios /= 0 ) then
        print *, "Cannot open file ", trim(filename), ". The specified file does not exist. Execution will be aborted..."
        stop
    end if

    if(.not.present(extent))then
        read(free_unit,*)str,prm%header%jmax
        read(free_unit,*)str,prm%header%imax
        read(free_unit,'(1a10,1f20.10)')str,prm%header%x0
        backspace(free_unit)
        read(free_unit,*)str,prm%header%x0string
        read(free_unit,'(1a10,1f20.10)')str,prm%header%y0
        backspace(free_unit)
        read(free_unit,*)str,prm%header%y0string
        read(free_unit,*)str,prm%header%cellsize
        read(free_unit,*,iostat=ios) str,prm%header%nan
        if(present(sim))call check_header(sim,prm%header,filename)
        allocate(prm%mat(prm%header%imax,prm%header%jmax))
        do i=1,size(prm%mat,1)
            read(free_unit,*, iostat=ierror) prm%mat(i,:)
            if (ierror /= 0) then
                print *, "A number type error was detected in file ", trim(filename), ". Execution will be aborted..."
            end if
        end do
    else
        read(free_unit,*)str,prm%header%jmax
        read(free_unit,*)str,prm%header%imax
        read(free_unit,'(1a10,1f20.10)')str,prm%header%x0
        read(free_unit,'(1a10,1f20.10)')str,prm%header%y0
        read(free_unit,*)str,prm%header%cellsize
        read(free_unit,*,iostat=ios)str,prm%header%nan
        if(present(sim))call check_header(sim,prm%header,filename)
        allocate(aux_parametro(prm%header%imax,prm%header%jmax))
        prm%header%jmax=extent%header%jmax
        prm%header%imax=extent%header%imax
        prm%header%x0=extent%header%x0
        prm%header%x0string=extent%header%x0string
        prm%header%y0=extent%header%y0
        prm%header%y0string=extent%header%y0string
        do i=1,size(aux_parametro,1)
            read(free_unit,*, iostat=ierror)aux_parametro(i,:)
            if (ierror /= 0) then
                print *, "A number type error was detected in file ", trim(filename), ". Execution will be aborted..."
            end if
        end do
        allocate(prm%mat(prm%header%imax,prm%header%jmax))
        prm%mat(1:size(prm%mat,1),1:size(prm%mat,2))= &
        aux_parametro(extent%i_north:extent%i_south,extent%j_west:extent%j_est)
        deallocate(aux_parametro)
    end if
    close(free_unit)

end subroutine read_grid_i

subroutine write_grid_r(filename, grid)
    ! save real grid in ascii format file *.ASC
   character(len=*), intent(in) ::filename
    type(grid_r),intent(in)::grid
    integer :: i, ios, free_unit
    ios=0

    ! exit if filename is empty
    if (trim(filename) == '') return

    open(newunit=free_unit, file=trim(filename), action="write", iostat=ios)
    if( ios /= 0 ) then
        print *, "Error opening file '", trim(filename), "'; iostat=", ios
        print *, 'Execution will be aborted...'
        stop
    end if
    write(free_unit,'(1a,1x,1i6)') 'ncols', size(grid%mat,2)
    write(free_unit,'(1a,1x,1i6)') 'nrows', size(grid%mat,1)
    write(free_unit, '(1a,1x,1a)') 'xllcorner',grid%header%x0string
    write(free_unit, '(1a,1x,1a)') 'yllcorner',grid%header%y0string
    write(free_unit, '(1a,1x,1f12.5)') 'cellsize',grid%header%cellsize
    write(free_unit, '(1a,1x,1f12.5)') 'NODATA_value  ',grid%header%nan
    do i=1, size(grid%mat,1)
!~          write(free_unit, '(*(1f12.5, 1x))') prm%mat(i,:)
        write(free_unit, *) grid%mat(i,:)
    end do
    close(free_unit)
    print*,"print file: ", trim(trim(filename))

end subroutine write_grid_r

subroutine write_grid_i(filename, grid)
    ! save integer grid in ascii format file *.ASC
   character(len=*), intent(in) ::filename
    type(grid_i),intent(in)::grid
    integer :: i, ios, free_unit
    ios=0

    ! exit if filename is empty
    if (trim(filename) == '') return

    open(newunit=free_unit, file=trim(filename), action="write", iostat=ios)
    if( ios /= 0 ) then
        print *, "Error opening file '", trim(filename), "'; iostat=", ios
        stop
    end if
    write(free_unit,'(1a,1x,1i6)') 'ncols', size(grid%mat,2)
    write(free_unit,'(1a,1x,1i6)') 'nrows', size(grid%mat,1)
    write(free_unit, '(1a,1x,1a)') 'xllcorner',grid%header%x0string
    write(free_unit, '(1a,1x,1a)') 'yllcorner',grid%header%y0string
    write(free_unit, '(1a,1x,1f12.5)') 'cellsize',grid%header%cellsize
    write(free_unit, '(1a,1x,1i10)') 'NODATA_value  ',grid%header%nan
    do i=1, size(grid%mat,1)
        write(free_unit,*) grid%mat(i,:)
    end do
    close(free_unit)
    print*,"print file: ", trim(trim(filename))


end subroutine write_grid_i

subroutine write_head_and_mat_i(filename, header, mat)
    ! save header and matrix of integers in ascii format file *.ASC
   character(len=*),intent(in)::filename
    integer,dimension(:,:),intent(in)::mat
    type(grid_header_i),intent(in)::header
    type(grid_i)::grid

    allocate(grid%mat(size(mat,1),size(mat,2)))
    grid%mat = mat
    grid%header%x0 = header%x0
    grid%header%x0string = header%x0string
    grid%header%y0 = header%y0
    grid%header%y0string = header%y0string
    grid%header%nan = header%nan
    grid%header%cellsize = header%cellsize
    call write_grid(filename, grid)
    deallocate(grid%mat)
end subroutine write_head_and_mat_i

subroutine write_header_and_mat_r(filename, titolo, matrice)
    ! save header and matrix of reals in ascii format file *.ASC
   character(len=*),intent(in)::filename
    real(dp),dimension(:,:),intent(in)::matrice
    type(grid_header_i),intent(in)::titolo
    type(grid_r)::grid

    allocate(grid%mat(size(matrice,1),size(matrice,2)))
    grid%mat = matrice
    grid%header%x0 = titolo%x0
    grid%header%x0string = titolo%x0string
    grid%header%y0 = titolo%y0
    grid%header%y0string = titolo%y0string
    grid%header%nan = real(titolo%nan)
    grid%header%cellsize = titolo%cellsize
    call write_grid(filename, grid)
    deallocate(grid%mat)
end subroutine write_header_and_mat_r

subroutine min_domain(domain_fn,extent,sim)
    ! find the min and max row and col of the simulation domain
    character(len=*),intent(in)::domain_fn
    type(bound),intent(out)::extent
    type(simulation),intent(inout)::sim
    type(grid_i)::domain_grid

    ! read the domain file
    call read_grid(domain_fn,domain_grid)
    extent%i_north=1
    extent%i_south=size(domain_grid%mat,1)
    extent%j_west=1
    extent%j_est=size(domain_grid%mat,2)

    extent%header%imax = extent%i_south - extent%i_north + 1
    extent%header%jmax = extent%j_est - extent%j_west + 1
    extent%header%x0 = domain_grid%header%x0
    extent%header%x0string = domain_grid%header%x0string
    extent%header%y0 = domain_grid%header%y0
    extent%header%y0string = domain_grid%header%y0string
    extent%header%cellsize = domain_grid%header%cellsize

    sim%imax=extent%header%imax
    sim%jmax=extent%header%jmax
    sim%cell_size=domain_grid%header%cellsize
    sim%x0=extent%header%x0
    sim%y0=extent%header%y0

end subroutine min_domain

subroutine check_header_i(sim,header_i,file_name)
    ! compare the header of the grid with settings in the simulation parameters
   type(simulation),intent(in)::sim
    type(grid_header_i),intent(in)::header_i
    character(len=*),intent(in)::file_name

    if(header_i%imax /= sim%imax)then
        print *, "Error in file '", trim(file_name), "': imax is not equal to domain. Execution will be aborted..."
        stop
    end if
    if(header_i%jmax /= sim%jmax)then
        print *, "Error in file '", trim(file_name), "': jmax is not equal to domain. Execution will be aborted..."
        stop
    end if
    if(abs(header_i%x0 - sim%x0) > 1D-9)then
        print *, "Error in file '", trim(file_name), "': x0 is not equal to domain. Execution will be aborted..."
        stop
    end if
    if(abs(header_i%y0 - sim%y0) > 1D-9)then
        print *, "Error in file '", trim(file_name), "': y0 is not equal to domain. Execution will be aborted..."
        stop
    end if
end subroutine check_header_i

subroutine check_header_r(sim,header_r,file_name)
    ! compare the header of the grid with settings in the simulation parameters
    type(simulation),intent(in)::sim
    type(grid_header_r),intent(in)::header_r
    character(len=*),intent(in)::file_name

    if(header_r%imax /= sim%imax)then
        print *, "Error in file '", trim(file_name), "': imax is not equal to domain. Execution will be aborted..."
        stop
    end if
    if(header_r%jmax /= sim%jmax)then
        print *, "Error in file '", trim(file_name), "': jmax is not equal to domain. Execution will be aborted..."
        stop
    end if
    if(abs(header_r%x0 - sim%x0) > 1D-9)then
        print *, "Error in file '", trim(file_name), "': x0 is not equal to domain. Execution will be aborted..."
        stop
    end if
    if(abs(header_r%y0 - sim%y0) > 1D-9)then
        print *, "Error in file '", trim(file_name), "': y0 is not equal to domain. Execution will be aborted..."
        stop
    end if
end subroutine check_header_r

subroutine overlay_domain_i(par_grid, domain_grid, par_name)
    ! Remove cells missing from the parameter map from the active domain (and vice versa)
    type(grid_i), intent(inout) :: domain_grid
    type(grid_i), intent(inout) :: par_grid
    character(len=*), intent(in) :: par_name

    if (any(par_grid%mat==par_grid%header%nan .and. domain_grid%mat/=domain_grid%header%nan)) then
        print *, "Warning: parameter map '",trim(par_name),"' does not have valid data for the entire active domain;&
               & the domain will be reduced to cells with valid data."
    end if
    where(par_grid%mat==par_grid%header%nan) domain_grid%mat=domain_grid%header%nan
    where(domain_grid%mat==domain_grid%header%nan) par_grid%mat=par_grid%header%nan
end subroutine overlay_domain_i

subroutine overlay_domain_r(par_grid, domain_grid, par_name)
    ! Remove cells missing from the parameter map from the active domain (and vice versa)
    type(grid_i), intent(inout) :: domain_grid
    type(grid_r), intent(inout) :: par_grid
    character(len=*), intent(in) :: par_name

    if (any(par_grid%mat==par_grid%header%nan .and. domain_grid%mat/=domain_grid%header%nan)) then
        print *, "Warning: parameter map '",trim(par_name),"' does not have valid data for the entire active domain;&
               & the domain will be reduced to cells with valid data."
    end if
    where(par_grid%mat==par_grid%header%nan) domain_grid%mat=domain_grid%header%nan
    where(domain_grid%mat==domain_grid%header%nan) par_grid%mat=par_grid%header%nan
end subroutine overlay_domain_r

subroutine set_default_par_i(a_grid,domain_grid,default_value)
    ! fill missing value with the value provided by the user - integer
    type(grid_i),intent(in)::domain_grid
    type(grid_i),intent(inout)::a_grid
    integer,intent(in)::default_value

    where(a_grid%mat==a_grid%header%nan .and. domain_grid%mat/=domain_grid%header%nan) a_grid%mat=default_value
end subroutine set_default_par_i

subroutine set_default_par_r(a_grid,domain_grid,default_value)
    ! fill missing value with the value provided by the user - real
    type(grid_i),intent(in)::domain_grid
    type(grid_r),intent(inout)::a_grid
    real(dp),intent(in)::default_value

    where(a_grid%mat==a_grid%header%nan .and. domain_grid%mat/=domain_grid%header%nan) a_grid%mat=default_value
end subroutine set_default_par_r

function id_to_par_r(id_grid,pars_list)
    ! return a matrix of real values
    ! choosing the values from the list provided (pars_list)
    ! base on the position in the list
    !%PS%: id_grid = 0 returns a value of 0 regardless of pars_list (irr_meth 0 means "not irrigable" and doesn't need actual parameters)
   type(grid_i),intent(in)::id_grid
    real(dp),dimension(:),intent(in)::pars_list

    real(dp),dimension(id_grid%header%imax, id_grid%header%jmax)::id_to_par_r
    integer::i,j

    id_to_par_r=real(id_grid%header%nan)
    where(id_grid%mat==0) id_to_par_r=0.0_dp
    forall(i=1:size(id_grid%mat,1), j=1:size(id_grid%mat,2), id_grid%mat(i,j)>0)
        id_to_par_r(i,j)=pars_list(id_grid%mat(i,j))
    end forall
end function id_to_par_r

function id_to_par_i(id_grid,pars_list)
    ! return a matrix of integer values
    ! choosing the values from the list provided (pars_list)
    ! base on the position in the list
    !%PS%: id_grid = 0 returns a value of 0 regardless of pars_list (irr_meth 0 means "not irrigable" and doesn't need actual parameters)
   type(grid_i),intent(in)::id_grid
    integer,dimension(:),intent(in)::pars_list

    integer, dimension(id_grid%header%imax, id_grid%header%jmax)::id_to_par_i
    integer::i,j

    id_to_par_i=id_grid%header%nan
    where(id_grid%mat==0) id_to_par_i=0
    forall(i=1:size(id_grid%mat,1), j=1:size(id_grid%mat,2), id_grid%mat(i,j)>0)
        id_to_par_i(i,j)=pars_list(id_grid%mat(i,j))
    end forall
end function id_to_par_i

end module mod_grid
