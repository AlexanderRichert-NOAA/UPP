program t1
  use grib2_module
  implicit none
  integer(4) :: len3, ifield3len
  integer(4) :: ifield3(19),igds(5)  
  logical ldfgrd  
  integer :: nx,ny
  integer lat1,lon1,lat2,lon2,lad,ds1

  print *, 'testing grib2_module...'

  call grib_info_init()

  len3 = 19
  call g2sec3tmpl40(nx,nY,lat1,lon1,lat2,lon2,lad,ds1,len3,igds,ifield3)

  call getgds(ldfgrd, len3, ifield3len, igds, ifield3)
  
  call grib_info_finalize()
  
  print *, 'SUCCESS!'
end program t1
