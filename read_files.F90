!Simple example program for reading in text files in fortran
!Files are sturctured as FixXXX.txt, where XXX indicates a sequential
!Identifier with three digits i.e. 001,002...101 etc.
!AUTHOR: Ethan J Carter | Sep 2023
program read_files
    implicit none
    integer :: i, d
    integer :: ioerr
    character(len=len("FixXXX.txt")) :: fname
    character(len=3) :: ichar

    open(unit=30, file="Output.txt", action="write", iostat=ioerr)
    if (ioerr /= 0) stop 1

    do i = 0, 999
        write(fname, '(A, I3.3, A)') "Fix", i, ".txt"
        open(unit = 40, file=fname, status="old", action="read", iostat=ioerr)
        print*, fname
        if (ioerr /= 0) cycle
        read(40, *) d
        write(30, *) d
        close(40)
    end do
end program read_files