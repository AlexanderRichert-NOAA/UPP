program t3
  use grib2_module
  use ctlblk_mod
  use xml_perl_data
  ! use ctlblk_mod, only : im,jm,im_jm,num_procs,me,ista,iend,jsta,jend,ifhr,sdat,ihrst,imin,    &
  !      mpi_comm_comp,ntlfld,fld_info,datapd,icnt,idsp
  implicit none
  include 'mpif.h'
  
  character(255) :: post_fname = 't1_post_fname'
  integer :: param_count, paramset_count
  integer :: i, j, k, ierr, nlvl

  ! Set up MPI.
  call setup_servers(me, num_procs, num_servers, mpi_comm_comp, mpi_comm_inter)

  if (me .eq. 0) then
     print *, 'testing gribit2(), num_procs is', num_procs
  endif

  ! This tests must be run on 1 or 4 procs.
  if (num_procs .ne. 4 .and. num_procs .ne. 1) stop 3

  first_grbtbl = .true. 
  call grib_info_init()
  if (trim(pset%sub_center) .ne. 'ncep_emc') stop 10
  !print *, pset

  im = 10
  jm = 10
  im_jm = im * jm
  cfld = 1
  ntlfld = 1
  ista = 1
  iend = 10
  jsta = 1
  jend = 10
  nrecout = 1
  j = 1
  allocate(datapd(1 : iend - ista + 1, 1 : jend - jsta + 1, nrecout + 100))
  do k = 1, nrecout + 100  
     do i = 1, iend + 1 - ista
        datapd(i, j, k) = 0.
     enddo
  end do
  allocate(fld_info(NRECOUT+100))
  do i=1,nrecout
     fld_info(i)%ifld     = i
     fld_info(i)%lvl      = 1
     fld_info(i)%lvl1     = 0
     fld_info(i)%lvl2     = 0
     fld_info(i)%ntrange  = 0
     fld_info(i)%tinvstat = 0
  enddo
  npset = 1
  param_count = 1
  paramset_count = 1
  nlvl = 5
  allocate(paramset(paramset_count))  
  allocate(paramset(paramset_count)%param(param_count))  
  allocate(paramset(paramset_count)%param(param_count)%scale_fact_fixed_sfc1(nlvl))  
  allocate(paramset(paramset_count)%param(param_count)%scale_fact_fixed_sfc2(nlvl))  
  allocate(paramset(paramset_count)%param(param_count)%scale(1))  
  allocate(paramset(paramset_count)%param(param_count)%level(nlvl))  
  pset   = paramset(1)
  paramset(1)%param(1)%pname = 'TMP'
  call gribit2(post_fname)

  call grib_info_finalize()

  ! We're done!
  call MPI_Finalize(ierr)
  if (me .eq. 0) then
     print *, '*** SUCCESS!'
  endif  
end program t3
