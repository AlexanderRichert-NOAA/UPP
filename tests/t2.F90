program t2
  use grib2_module
  implicit none
  
  integer(4) :: len3, ifield3len
  integer(4) :: ifield3(19),igds(5)
  integer(4) :: expected_ifield3(19) = (/ 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 48, 0, 0, 0, 0, 0 /)
  integer(4) :: expected_ifield3_2(19) = (/ 6, 0, 0, 0, 0, 0, 0, 1152, 576, 0, 0, 89761000, 0, &
       48, -89761000, 359687000, 313000, 288, 0 /)
  
  logical ldfgrd  
  integer :: nx,ny
  integer lat1,lon1,lat2,lon2,lad,ds1
  integer :: i
  character(255) :: post_fname = 't1_post_fname'

  print *, 'testing grib2_module...'

  call grib_info_init()
  if (trim(pset%sub_center) .ne. 'ncep_emc') stop 10
  !print *, pset

  len3 = 19
  call getgds(ldfgrd, len3, ifield3len, igds, ifield3)
  do i = 1, 19
     if (ifield3(i) .ne. expected_ifield3(i)) stop 20
  end do
  
  call g2sec3tmpl40(nx,nY,lat1,lon1,lat2,lon2,lad,ds1,len3,igds,ifield3)
  do i = 1, 19
     if (ifield3(i) .ne. expected_ifield3_2(i)) stop 20
  end do

  call grib_info_finalize()
  
  print *, 'SUCCESS!'
end program t2
