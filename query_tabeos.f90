!Module for pulling particle data from a tabulated eos used for icooling-8 (Stamatellos 2007)
!Looks up particle data based on density and energy and obtains temperature, mu etc.
!AUTHOR: Ethan J Carter | Sep 2023
module query_tabeos

 implicit none !Fortran tax

 public read_tables, read_particles, query_snapshot, query_all_files

 private

 contains

!Read in eos table, currently only configured for lombardi eos table
 subroutine read_tables(tabfile, rho, temp, specific_energy,mu,pseudo_ro,ro,planck)
!                                      ro=Rosseland opacity, planck=Planck Opacity
   real, intent(inout)              :: rho(:),temp(:),specific_energy(:),mu(:),pseudo_ro(:),ro(:),planck(:) 
   character(len=*), intent(in)     :: tabfile
   integer                          :: i, no_lines=0, fid !fid is the unit ID for the file, assigned later

   open(newunit=fid,file=tabfile,status='old',action='read')
   print "(A)",'Target file is '//trim(tabfile)

   read(fid, *)
   read(fid, *)
   read(fid, *)
   read(fid, *)
   read(fid, *) !Skip headers

   do i=1,size(rho)
      read(fid,*) rho(i),temp(i),specific_energy(i),mu(i),pseudo_ro(i),ro(i),planck(i)
!      no_lines = no_lines + 1
   enddo

   close(fid)

!   print*,'(A)',no_lines
!   print*,'--------------------------------------------'
!   print*,'(E4.2)',temp
!   print*,'--------------------------------------------'

!   print*,'(E6.2)',temp(1)
!   print*,'(E6.2)',rho(1)
!   print*,'(E6.2)',specific_energy(1)

 end subroutine


!Routine for accessing particle data from phantom analysis output (maxparts.f90)
!Should pull density and specific energy from particle
 subroutine read_particles(partfile, rhopart, energpart)

   real, intent(inout)          :: rhopart(:),energpart(:)
   character(len=*), intent(in) :: partfile
   integer                      :: i, fid

   open(newunit=fid, file=partfile, status='old',action='read')
   print "(A)",'Target particle file is '//trim(partfile)

   read(fid,*)
   read(fid,*) !Skip headers

   do i=1,size(rhopart)
    read(fid,*) rhopart(i), energpart(i)
!    print*, rhopart(i), energpart(i)
   enddo

   close(fid)

 end subroutine

 subroutine query_snapshot(partfile, tabfile, rhopart, energpart, rho, specific_energy, mu, pseudo_ro, ro, planck, temp, output)

   real, intent(inout)          :: rhopart(:), energpart(:), rho(:), specific_energy(:), temp(:), &
                                      mu(:),pseudo_ro(:),ro(:),planck(:)
   character(len=*), intent(in) :: partfile, tabfile, output
   integer                      :: i=1, fid, binlower, idx
   real, allocatable            :: bin(:)

   call read_particles(partfile,rhopart,energpart)
   call read_tables(tabfile,rho,temp,specific_energy,mu,pseudo_ro,ro,planck)

   open(newunit=fid, file=output, status='replace',action='write')

   write(fid,"('#',5(1x,'[',i2.2,1x,a11,']',2x))") &
   1,'Density'     , &
   2,'EOS Density' , &
   3,'Energy'      , &
   4,'EOS Energy'  , &
   5,'Temperature'

   !Loop should check for correct bin for particle density and write rho, energy, temp to outfile
    do i=1,size(rhopart)

      binlower   = minloc(abs(rho-rhopart(i)), dim=1)              !gives start of bin with closest value
      bin        = specific_energy(binlower:(binlower+999))        !use bin index to extract relevant rows for energy query, bins should be of size 1000
      idx        = binlower + minloc(abs(bin-energpart(i)), dim=1) !gives index of closest specific energy inside of bin
      write(fid,'(4((E16.8),1x),(F15.7))') rhopart(i),rho(idx),energpart(i),specific_energy(idx),temp(idx) !write to file

    enddo

    !print*, idx  - binlower
    !print*, temp(idx)

   close(fid)

 end subroutine

 !subroutine read_maxvals(rhopart, energpart, rho, specific_energy, mu, pseudo_ro, ro, planck)

  !real, intent(inout)     :: rhopart


 !end subroutine


 subroutine query_all_files(tabfile, rhopart, energpart, rho, specific_energy, mu, pseudo_ro, ro, planck, temp)

  real, intent(inout)          :: rhopart(:), energpart(:), rho(:), specific_energy(:), temp(:), &
  mu(:),pseudo_ro(:),ro(:),planck(:)
  character(len=*), intent(in) :: tabfile
  character(len=9) :: fname
  character(len=14) :: outname
  integer                      :: i, ioerr, inid

    do i = 1, 399
        write(fname, '(A, I3.3)') "part00", i
        write(outname, '(A, I3.3)') "queryeos_00", i
        print*, fname
        !check if particle file exists
        open(newunit=inid, file=fname, status="old", action="read", iostat=ioerr)
        if (ioerr /= 0) cycle
        close(inid)

        call query_snapshot(fname,tabfile, rhopart, energpart, rho, specific_energy, mu, pseudo_ro, ro, planck, temp, outname)

    end do

 end subroutine

end module
