!Main program for reading in eos tables
program main

 use query_tabeos, only:read_tables, read_particles, query_snapshot, query_all_files
 implicit none
 real   :: rho(261000),temp(261000),specific_energy(261000),mu(261000), &
           pseudo_ro(261000),ro(261000),planck(261000), rhopart(100000), part_energy(100000)

 !call read_tables('eos.BL.lom.dat',rho,temp,specific_energy,mu,pseudo_ro,ro,planck)
 !call read_particles('part00314',rhopart,part_energy)
 !call query_eos('part00*','eos.BL.lom.dat', rhopart, part_energy, rho, specific_energy, mu, pseudo_ro, ro, planck, temp, 'new_output.dat')
 call query_all_files('eos.BL.lom.dat', rhopart, part_energy, rho, specific_energy, mu, pseudo_ro, ro, planck, temp)

end program main
