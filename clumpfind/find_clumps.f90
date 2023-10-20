!Module to locate and provide outfiles for clumps based on 
!positions obtained from specific output tracking algorithm
!AUTHOR: Ethan J Carter | Sep 2023

module data_types

  implicit none

  public

  !Define a derived data type for clumps
  type :: clump_class
    real(8) :: num, pid, rho, x, y, z
  end type clump_class

  !Define a derived data type for sinks
  type :: sink_class
    real(8) :: num, x, y, z
  end type sink_class

  !Define a derived data type for particles
  type particle_class
    integer :: pid
    real(8) :: x,y,z,mass,h,rho,temp,vx,vy,vz,u,mu,div,poten,dt
    character(len=1) :: type
  end type particle_class

  type fragment_class
    integer :: pid
    real(8) :: x,y,z,mass,h,rho,temp,vx,vy,vz,u,r
    character(len=1) :: type
  end type fragment_class

end module data_types

module find_clumps

 use :: data_types

 implicit none !Fortran Tax

 public read_nclumps_and_nsinks, read_clumpdata, locate_clump_neighbours, read_dumpfile

 private

 contains

!Read in clump data from tracking algorithm
 subroutine read_nclumps_and_nsinks(datfile)

   integer :: nclumps, nsinks, i
   character(len=*), intent(in) :: datfile
   integer :: fid, clumpID, ierr
   character(len=30) :: input_string, str, output
   type(clump_class), dimension(:), allocatable :: clumps
   type(sink_class), dimension(:), allocatable :: sinks
   type(particle_class), dimension(100000) :: particles
   type(fragment_class), dimension(1000) :: fragment

   !Read in existsing clumps_and_sinks0XX.dat file
   open(newunit=fid, file=datfile,status='old', action='read')
   print "(A)",'Target file is '//trim(datfile)


   !Read line, pick out character and convert to int
   read(fid,'(A)') input_string

   str =  input_string(30:30)

   read(str,*,iostat=ierr) nclumps

   print*, 'Number of clumps: ', nclumps

   read(fid,'(A)') input_string

   str = input_string(29:29)

   read(str,*,iostat=ierr) nsinks

   print*, 'Number of sinks: ', nsinks

   !Close file
   close(fid)

   allocate(clumps(nclumps), sinks(nsinks))

   call read_clumpdata(datfile,nclumps, nsinks, clumps, sinks)

   call locate_clump_neighbours(nclumps,nsinks,clumps,sinks)

   call read_dumpfile('disc_00019.ascii',100000, particles)

   ! i=1, nclumps

   do clumpID=1,nclumps

    !Clean fragment for next iteration
    do i = 1, 1000
      fragment(i)%pid = 0
      fragment(i)%x = 0.0
      fragment(i)%y = 0.0
      fragment(i)%z = 0.0
      fragment(i)%mass = 0.0
      fragment(i)%h = 0.0
      fragment(i)%rho = 0.0
      fragment(i)%temp = 0.0
      fragment(i)%vx = 0.0
      fragment(i)%vy = 0.0
      fragment(i)%vz = 0.0
      fragment(i)%u = 0.0
      fragment(i)%r = 0.0
    end do

    write(output, '(A,I0,A)') 'F',clumpID,'.dat'

    print*, 'Outfile is: ', output

    call extract_fragment(clumps,particles,nclumps,100000,clumpID,fragment)

    print*, 'Fragments extracted, writing...'

    call write_fragment_data(clumps,nclumps,clumpID,fragment,output)

    print*, 'Done. Moving to next fragment...'

   enddo

 end subroutine read_nclumps_and_nsinks

 subroutine read_clumpdata(datfile,nclumps, nsinks, clumps, sinks)

   character(len=*), intent(in) :: datfile
   integer, intent(in) :: nclumps, nsinks
   type(clump_class), intent(out), dimension(nclumps) :: clumps
   type(sink_class), intent(out), dimension(nsinks) :: sinks
   integer :: fid, i, ierr
   character(len=100) :: str

   !Read in existing clumps_and_sinks0XX.dat file
   open(newunit=fid, file=datfile,status='old', action='read')
   !print "(A)",'Target file is '//trim(datfile)

   !Ignore header lines
   read(fid, *)
   read(fid, *)

   print*, 'Reading in clumps...'

   do i = 1,nclumps
    !Read clump data
    read(fid, *, iostat=ierr) clumps(i)%num, clumps(i)%pid, clumps(i)%rho, clumps(i)%x, clumps(i)%y, clumps(i)%z
    !Fail state if line cannot be read, exit loop
    if (ierr /= 0) then
      write(*,*) "Error reading line ", i
      write(*,*) "Problematic line content: ", str
      exit
    end if

    !Print the values as needed clumps(i)%
    !print*, "Line ", i, ":", clumps(i)
   enddo

   print*, 'Reading in sinks...'

   do i = 1,nsinks
    !Read sink data
    read(fid, *, iostat=ierr) sinks(i)%num, sinks(i)%x, sinks(i)%y, sinks(i)%z
    !Fail state if line cannot be read, exit loop
    if (ierr /= 0) then
      write(*,*) "Error reading line ", i
      write(*,*) "Problematic line: ", str
      exit
    end if

    !Print the values as needed sinks(i)%
    !print*, "Line ", i, ":", sinks(i)
   enddo

   close(fid)

 end subroutine read_clumpdata

 subroutine locate_clump_neighbours(nclumps,nsinks,clumps,sinks)
  integer, intent(in) :: nclumps, nsinks
  type(clump_class), dimension(nclumps), intent(in) :: clumps
  type(sink_class), dimension(nsinks), intent(in) :: sinks
  integer :: i, fid_b

 end subroutine locate_clump_neighbours

 subroutine read_dumpfile(dumpfile, npart, particles)
  integer, intent(in) :: npart
  character(len=*), intent(in) :: dumpfile
  type(particle_class), dimension(npart), intent(out) :: particles
  integer :: fid2, i, ierr
  !character(len=100) :: str
  !real :: x, y, z, mass, rho, temp, vx, vy, vz, u, mu, value1, value2, value3, value4, value5

  !Read in existing clumps_and_sinks0XX.dat file
  open(newunit=fid2, file=dumpfile,status='old', action='read')
  print "(A)",'Target snapshot is '//trim(dumpfile)

  print*,  'Skipping headers...'

  !Ignore header lines x12
  read(fid2, *)
  read(fid2, *)
  read(fid2, *)
  read(fid2, *)
  read(fid2, *)
  read(fid2, *)
  read(fid2, *)
  read(fid2, *)
  read(fid2, *)
  read(fid2, *)
  read(fid2, *)
  read(fid2, *)
  
  print*,  'Reading in particle data...'
  !read(fid2,'(A)') str
  !print*, str

  do i=1, npart

    particles(i)%pid = i

    read(fid2, *, iostat=ierr) particles(i)%x,particles(i)%y,particles(i)%z,particles(i)%mass,particles(i)%h,particles(i)%rho, &
    particles(i)%temp,particles(i)%vx,particles(i)%vy,particles(i)%vz,particles(i)%u, &
    particles(i)%mu,particles(i)%div,particles(i)%poten,particles(i)%dt,particles(i)%type

    if (ierr == 1) then
      ! Error reading line, exit
      print *, "Error reading line"
      exit
    endif

    !print*, i
    !print*, '______________'
    !print*, particles(i)
    !print*, '==============='

  enddo

  print*, 'Dumpfile read into particles object successfully'

  close(fid2)

  !TEST
  !do i=1, 3
    ! Read the line with the specified format
    !READ(fid2, *) x, y, z, mass, rho, temp, vx, vy, vz, u, mu, value1, value2, value3, value4, value5

    ! Print the values to check
    !PRINT *, "x =", x
    !PRINT *, "y =", y
    !PRINT *, "z =", z
    !PRINT *, "mass =", mass
    !PRINT *, "rho =", rho
    !PRINT *, "temp =", temp
    !PRINT *, "vx =", vx
    !PRINT *, "vy =", vy
    !PRINT *, "vz =", vz
    !PRINT *, "u =", u
    !PRINT *, "mu =", mu
    !PRINT *, "value1 =", value1
    !PRINT *, "value2 =", value2
    !PRINT *, "value3 =", value3
    !PRINT *, "value4 =", value4
    !PRINT *, "value5 =", value5
    !print*, '============================'

  !enddo

 end subroutine read_dumpfile

 subroutine extract_fragment(clumps,particles,nclumps,npart,clumpID,fragment)
  integer, intent(in) :: nclumps, npart, clumpID
  type(clump_class), dimension(nclumps), intent(in) :: clumps
  type(particle_class), dimension(npart), intent(in) :: particles
  type(fragment_class), dimension(1000) :: fragment
  integer :: i
  integer :: j
  real(kind=8) :: dx_sq, dy_sq, dz_sq, r_sq, r

    ! Initialize fragment for the current clump
  do i = 1, 1000
    fragment(i)%pid = 0
    fragment(i)%x = 0.0
    fragment(i)%y = 0.0
    fragment(i)%z = 0.0
    fragment(i)%mass = 0.0
    fragment(i)%h = 0.0
    fragment(i)%rho = 0.0
    fragment(i)%temp = 0.0
    fragment(i)%vx = 0.0
    fragment(i)%vy = 0.0
    fragment(i)%vz = 0.0
    fragment(i)%r = 0.0
  end do

  !print*, clumps(clumpID)

  !Calculate distance to centre of fragment and add to fragment object
  j=1
  do i=1, npart
    dx_sq = (particles(i)%x - clumps(clumpID)%x)**2
    dy_sq = (particles(i)%y - clumps(clumpID)%y)**2
    dz_sq = (particles(i)%z - clumps(clumpID)%z)**2
    r_sq  = dx_sq + dy_sq + dz_sq
    r = sqrt(r_sq)

    !print*, r
    if (r < 5) then

      print*, 'Adding particle...'

      fragment(j)%pid = particles(i)%pid
      fragment(j)%x = particles(i)%x
      fragment(j)%y = particles(i)%y
      fragment(j)%z = particles(i)%z
      fragment(j)%mass = particles(i)%mass
      fragment(j)%h = particles(i)%h
      fragment(j)%rho = particles(i)%rho
      fragment(j)%temp = particles(i)%temp
      fragment(j)%vx = particles(i)%vx
      fragment(j)%vy = particles(i)%vy
      fragment(j)%vz = particles(i)%vz
      fragment(j)%u = particles(i)%u
      fragment(j)%r = r

      !print*, fragment(j)

      j = j+1
    endif
  enddo

  print*, 'done'

 end subroutine extract_fragment

 subroutine write_fragment_data(clumps,nclumps,clumpID,fragment, output)
 integer, intent(in) :: nclumps, clumpID
 type(clump_class), dimension(nclumps), intent(in) :: clumps
 type(fragment_class), dimension(1000), intent(in) :: fragment
 integer :: fid3, i
 character(len=*), intent(in) :: output !Outfile name

 open(newunit=fid3, file=output, status='replace',action='write')

 !Write out basic clump information
 write(fid3,*) '#Fragment: ', clumpID
 write(fid3,"('#',5(1x,'[',1x,a11,']',2x))") &
 'x'     , &
 'y' , &
 'z'      , &
 'rho'  , &
 'pid'

 write(fid3,'(5((E16.8),1x))')  clumps(clumpID)%x, clumps(clumpID)%y,clumps(clumpID)%z,clumps(clumpID)%rho, clumps(clumpID)%pid

 write(fid3,"('#',12(1x,'[',i2.2,1x,a11,']',2x))") &
 1, 'pid'    , &
 2, 'x'      , &
 3, 'y'      , &
 4, 'z'      , &
 5, 'h'      , &
 6, 'vx'     , &
 7, 'vy'     , &
 8, 'vz'     , &
 9, 'r'      , &
 10,'mass'   , &
 11,'rho'    , &
 12,'temp'

 do i=1,1000
  write(fid3,'(I8,8(F10.2),2(E16.8),F10.2,1x)') fragment(i)%pid,fragment(i)%x,fragment(i)%y,fragment(i)%z,fragment(i)%h, &
                                 fragment(i)%vx,fragment(i)%vy,fragment(i)%vz,fragment(i)%r,fragment(i)%mass, &
                                 fragment(i)%rho,fragment(i)%temp
  !print*, 'Writing line', i, 'to file...'
  !print*, fragment(i)
 enddo

 close(fid3)

 print*, 'Left the loop...'

 end subroutine write_fragment_data

end module find_clumps
