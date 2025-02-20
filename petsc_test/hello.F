program main

#include "include/finclude/petsc.h"

  PetscErrorCode ierr

  call PetscInitialize ( PETSC_NULL_CHARACTER, ierr )

  call PetscPrintf ( PETSC_COMM_WORLD, 'Hello, world!', ierr )

  call PetscFinalize ( ierr )

  stop
end
