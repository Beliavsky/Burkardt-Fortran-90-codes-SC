program main

   use rand_generator
   use pdf_density
   use dream
   
   call gen_uniform_01()

   call read_vars()

   call init_prior()
 
   call init_jumprate()

   call init_GR()

   call init_chain()

   call DREAM_algm()

   call OutputRestart()

end program
