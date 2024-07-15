program t3
  use grib2_module
  use ctlblk_mod
  ! use ctlblk_mod, only : im,jm,im_jm,num_procs,me,ista,iend,jsta,jend,ifhr,sdat,ihrst,imin,    &
  !      mpi_comm_comp,ntlfld,fld_info,datapd,icnt,idsp
  implicit none
  include 'mpif.h'
  
  character(255) :: post_fname = 't1_post_fname'
  integer :: ierr
  

  ! Set up MPI.
  call setup_servers(me, num_procs, num_servers, mpi_comm_comp, mpi_comm_inter)

  if (me .eq. 0) then
     print *, 'testing gribit2(), num_procs is', num_procs
  endif

  ! This tests must be run on 1 or 4 procs.
  if (num_procs .ne. 4 .and. num_procs .ne. 1) stop 3

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
