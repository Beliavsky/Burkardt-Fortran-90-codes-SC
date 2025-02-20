include ${PETSC_DIR}/bmake/common/base

hello: hello.o
	-${FLINKER} -o hello hello.o ${PETSC_FORTRAN_LIB} ${PETSC_LIB}
	${RM} hello.o
ex5: ex5.o
	-${FLINKER} -o ex5 ex5.o ${PETSC_FORTRAN_LIB} ${PETSC_LIB}
	${RM} ex5.o

