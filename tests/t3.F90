program t3
  use grib2_module
  use ctlblk_mod, only : im,jm,im_jm,num_procs,me,ista,iend,jsta,jend,ifhr,sdat,ihrst,imin,    &
       mpi_comm_comp,ntlfld,fld_info,datapd,icnt,idsp
  
  implicit none
  include 'mpif.h'
  
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
  integer :: my_rank, ntasks, ierr, num_servers, mpi_comm_inter
  

  ! Set up MPI.
  call setup_servers(me, num_procs, num_servers, mpi_comm_comp, mpi_comm_inter)

  if (me .eq. 0) then
     print *, 'testing grib2_module, num_procs is', num_procs
  endif
  if (num_procs .ne. 4) stop 3

  call grib_info_init()
  if (trim(pset%sub_center) .ne. 'ncep_emc') stop 10
  !print *, pset

  call gribit2(post_fname)

  call grib_info_finalize()

  ! We're done!
  call MPI_Finalize(ierr)
  if (me .eq. 0) then
     print *, '*** SUCCESS!'
  endif  
end program t3
