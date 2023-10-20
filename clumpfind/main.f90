!main_file for clump_analysis files
program main

    use find_clumps, only: read_nclumps_and_nsinks, read_clumpdata
    implicit none

    call read_nclumps_and_nsinks('clumps_and_sinks019.dat')

end program main
