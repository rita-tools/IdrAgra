module mod_constants
implicit none

! Symbolic names for kind types of single- and double-precision reals
!integer, parameter :: sp = kind(1.0)
integer, parameter :: dp = kind(1.0D0)

! set default NAN values
integer, parameter :: nan_i = -9999
real(dp), parameter :: nan_r = -9999.0

real(dp), parameter :: cost_fwEva = 1 ! default fwEvaporation
real(dp),dimension(24),parameter :: cost_hrs =  (/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24/)

! %EAC%: add CN values for 72% impervious area
! TODO: set CN externally
integer, dimension(10,3,4), parameter:: tabCN = reshape( & ! CN2 table
    & (/74, 65, 61, 58, 30, 32, 99, 77, 25, 81, &    ! # (i,1,1)
    &    0, 72, 65, 66,  0, 43,  0,  0, 36, 0, &    ! # (i,2,1)
    &    0,  0,  0,  0,  0, 57,  0,  0, 45, 0, &    ! # (i,3,1)
    &   83, 75, 73, 72, 58, 58, 99, 86, 55, 88, &    ! # (i,1,2)
    &    0, 81, 76, 77,  0, 65,  0,  0, 60, 0, &    ! # (i,2,2)
    &    0,  0,  0,  0,  0, 73,  0,  0, 66, 0, &    ! # (i,3,2)
    &   88, 82, 81, 81, 71, 72, 99, 91, 70, 91, &    ! # (i,1,3)
    &    0, 88, 84, 85,  0, 76,  0,  0, 73, 0, &    ! # (i,2,3)
    &    0,  0,  0,  0,  0, 82,  0,  0, 77, 0, &     ! # (i,3,3)
    &   90, 86, 84, 85, 78, 79, 99, 94, 77, 93, &    ! # (i,1,4)
    &    0, 91, 88, 89,  0, 82,  0,  0, 79, 0, &    ! # (i,2,4)
    &    0,  0,  0,  0,  0, 86,  0,  0, 83, 0 /), &  ! # (i,3,4)
    & (/10,3,4/))                                 ! final form
    ! columns (1° number):  Cover type:
            ! 1 - Crop Residue Cover; 2 - Row crops; 3 - Small grain
            ! 4 - Close-seeded or broadcast legumes or rotation meadow; 5 - Meadow; 6 - Woods – grass combination (orchard or tree farm)
            ! 7 - Rice; 8 - Fallow - Bare soil; 9 - Woods; 10 - Urban (72% impervious area)
    ! rows (2° number):     Hydrological condition
    ! levels (3° number):   Hydrological group

integer, dimension(12), parameter:: tmax_time = &
    & (/14, 14, 14, 15, 15, 16, 15, 15, 15, 14, 14, 14/)    ! Tmax time for the months from Jan to Dec

integer, dimension(12), parameter:: tmin_time = &
    & (/6, 6, 5, 5, 4, 4, 4, 5, 5, 6, 6, 7/)                ! Tmin time for the months from Jan to Dec

! set pi value
real(dp), parameter :: pi= acos(-1.)

real(dp), parameter :: res_canopy_std = 70.                 ! Standard canopy resistance (alfalfa) for ET0 calculation

integer, parameter :: seconds_per_day = 24 * 60 * 60

end module mod_constants
