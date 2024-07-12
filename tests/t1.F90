program t1
  use grib2_module
  implicit none

  call grib_info_init()
  
  print *, 'testing...'
  print *, 'SUCCESS!'
end program t1
