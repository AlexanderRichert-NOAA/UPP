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
  integer :: check_test_file
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

  ! Now open the file and check it.
  ierr = check_test_file(post_fname)
  if (ierr .ne. 0) stop 100

  ! We're done!
  call MPI_Finalize(ierr)
  if (me .eq. 0) then
     print *, '*** SUCCESS!'
  endif  
end program t3

! Check the test file for correctness.
integer function check_test_file(file_name)
  use bacio_module
  implicit none
  
  character(255), intent(in) :: file_name
  integer*8 :: iseek8, mseek8, lskip8, lgrib8   
  integer :: lugb, iret

  ! Open the test file.
  lugb = 11
  call baopenr(lugb, trim(file_name), iret)
  if (iret .ne. 0) stop 10

  iseek8 = 0
  mseek8 = 100
  call skgb8(lugb, iseek8, mseek8, lskip8, lgrib8)  
  !print *, 'lskip8', lskip8, 'lgrib8', lgrib8
  if (lskip8 .ne. 0 .or. lgrib8 .ne. 192) stop 200

  ! Close the test file.
  call baclose(lugb, iret)
  if (iret .ne. 0) stop 51
  
  check_test_file = 0
  
end function check_test_file
