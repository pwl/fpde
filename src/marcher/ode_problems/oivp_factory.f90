module oivp_factory
   
   use class_ode_iv_problem
   use class_bruss_oivp
   use class_eulr_oivp
   use class_aren_oivp
   use class_stiff1_oivp

   contains
      
      function oivp_new(id) result(oivp)
         class(ode_iv_problem), pointer :: oivp
         character(len=*)               :: id

         select case(trim(id))
         case( "BRUSSELATOR" )
            allocate( bruss_oivp :: oivp )
         case( "EULER" )
            allocate( eulr_oivp :: oivp )
         case( "AREN" )
            allocate( aren_oivp :: oivp )
         case( "STIFF1" )
            allocate( stiff1_oivp :: oivp )
         case default
            print *, "oivp_new: invalid id"
            nullify(oivp)
         end select
      end function oivp_new

end module oivp_factory
