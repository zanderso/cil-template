# Macros for OCaml.

# Open issues:
# - Is it better to connect the generated variables to targets?  Scoping rules
#   are unknown to me, but I suspect that targets are global, while variables
#   may not be.
# - Is it really the case that I need to attach so many properties to the
#   targets just to be able to navigate them?
# - It'd be great to be able to list OCaml in the call to project()

# {{{1 set up variables
set( CMAKE_OCAML_COMPILER ${CMAKE_OCAML_OCAMLFIND} ocamlc
    CACHE STRING "Command used for bytecode targets."
    )

set( CMAKE_OCAML_COMPILER_OPT ${CMAKE_OCAML_OCAMLFIND} ocamlopt
    CACHE STRING "Command used for optimized targets."
    )

set( CMAKE_OCAML_DEP ${CMAKE_OCAML_OCAMLFIND} ocamldep -modules
    CACHE STRING "Command used to generate dependencies."
    )

set( CMAKE_OCAML_QUERY ${CMAKE_OCAML_OCAMLFIND} query
    CACHE STRING "Command used to find OCaml libraries."
    )

set( CMAKE_OCAML_PKGINSTALLDIR "lib/ocaml/site-lib"
    CACHE STRING "Default location for package install."
    )

mark_as_advanced(
    CMAKE_OCAML_COMPILER
    CMAKE_OCAML_COMPILER_OPT
    CMAKE_OCAML_DEP
    CMAKE_OCAML_QUERY
    )

# {{{1 find_ocaml_package
# find an OCaml package
macro( find_ocaml_package pkgname )
    string( TOUPPER ${pkgname} pkgname_uc )

    # find the package
    execute_process(
        COMMAND ${CMAKE_OCAML_QUERY} ${pkgname}
        OUTPUT_VARIABLE OCAML_${pkgname_uc}_INCLUDE_DIR
        OUTPUT_STRIP_TRAILING_WHITESPACE
        )
    execute_process(
        COMMAND ${CMAKE_OCAML_QUERY} -format "%v" ${pkgname}
        OUTPUT_VARIABLE OCAML_${pkgname_uc}_VERSION
        OUTPUT_STRIP_TRAILING_WHITESPACE
        )

    find_package_handle_standard_args( ${pkgname} DEFAULT_MSG
        OCAML_${pkgname_uc}_VERSION
        OCAML_${pkgname_uc}_INCLUDE_DIR
        )

    #export variables for found package
    set( OCAML_${pkgname_uc}_INCLUDE_DIR ${OCAML_${pkgname_uc}_INCLUDE_DIR}
        CACHE PATH ""
        )
    set( OCAML_${pkgname_uc}_LIBRARY_DIR ${OCAML_${pkgname_uc}_INCLUDE_DIR}
        CACHE PATH ""
        )
    mark_as_advanced(
        OCAML_${pkgname_uc}_INCLUDE_DIR
        OCAML_${pkgname_uc}_LIBRARY_DIR
        )
endmacro()

# {{{1 ocaml_trace
macro( ocaml_trace msg lst )
    message( STATUS "${msg}" )
    if( lst )
        foreach( i ${lst} )
            message( STATUS "   ${i}" )
        endforeach()
    endif()
endmacro()

# {{{1 ocaml_parse_arguments
# parse macro arguments
# copied from http://www.cmake.org/Wiki/CMakeMacroParseArguments
macro( ocaml_parse_arguments prefix arg_names option_names )
    set( default_args )
    foreach( arg_name ${arg_names} )
        set( ${prefix}_${arg_name} )
    endforeach( arg_name )
    foreach( option ${option_names} )
        set( ${prefix}_${option} FALSE )
    endforeach( option )

    set( current_arg_name DEFAULT_ARGS )
    set( current_arg_list )
    foreach( arg ${ARGN} )            
        set( larg_names ${arg_names} )    
        list( FIND larg_names "${arg}" is_arg_name )                   
        if( is_arg_name GREATER -1 )
            set( ${prefix}_${current_arg_name} ${current_arg_list} )
            set( current_arg_name ${arg} )
            set( current_arg_list )
        else( is_arg_name GREATER -1 )
            set( loption_names ${option_names} )    
            list( FIND loption_names "${arg}" is_option )            
            if( is_option GREATER -1 )
                set( ${prefix}_${arg} TRUE )
            else( is_option GREATER -1 )
                set( current_arg_list ${current_arg_list} ${arg} )
            endif()
        endif()
    endforeach()
    set( ${prefix}_${current_arg_name} ${current_arg_list} )
endmacro()

# {{{1 ocaml_set_target_deplibs
# collect dependencies on libraries which are targets
macro( ocaml_set_target_deplibs target )
    # collect the libs which are targets
    unset( OCAML_${target}_TARGET_DEPLIBS )
    foreach( lib ${OCAML_${target}_LIBRARIES} )
        list( APPEND OCAML_${target}_TARGET_DEPLIBS ocaml.${lib} )
    endforeach()
endmacro()

# {{{1 ocaml_set_target_translibs
macro( ocaml_set_target_translibs target )
    set( OCAML_${target}_TARGET_TRANSLIBS ${OCAML_${target}_TARGET_DEPLIBS} )
    foreach( lib ${OCAML_${target}_LIBRARIES} )
        get_target_property( translibs ocaml.${lib} TARGET_TRANSLIBS )
        list( APPEND OCAML_${target}_TARGET_TRANSLIBS ${translibs} )
    endforeach()
    if( OCAML_${target}_TARGET_TRANSLIBS )
        list( REMOVE_DUPLICATES OCAML_${target}_TARGET_TRANSLIBS )
    endif()
endmacro()

# {{{1 ocaml_set_target_transpkgs
macro( ocaml_set_target_transpkgs target )
    set( OCAML_${target}_TARGET_TRANSPKGS ${OCAML_${target}_PACKAGES} )
    foreach( lib ${OCAML_${target}_LIBRARIES} )
        get_target_property( transpkgs ocaml.${lib} TARGET_TRANSPKGS )
        list( APPEND OCAML_${target}_TARGET_TRANSPKGS ${transpkgs} )
    endforeach()
    if( OCAML_${target}_TARGET_TRANSPKGS )
        list( REMOVE_DUPLICATES OCAML_${target}_TARGET_TRANSPKGS )
    endif()
endmacro()

# {{{1 ocaml_set_include_dirs
# set up include dirs for the target based on the target libs it depends on
macro( ocaml_set_include_dirs target )
    # base value is the target's own build output dir
    set( OCAML_${target}_INCLUDE_DIRS
        "${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/ocaml.${target}.dir"
        )
    foreach( ltarget ${OCAML_${target}_TARGET_DEPLIBS} )
        get_target_property( obj_dir ${ltarget} OBJECT_DIRECTORY )
        list( APPEND OCAML_${target}_INCLUDE_DIRS ${obj_dir} )
    endforeach()
    get_directory_property( include_dirs INCLUDE_DIRECTORIES )
    list( APPEND OCAML_${target}_INCLUDE_DIRS ${include_dirs} )
    list( REMOVE_DUPLICATES OCAML_${target}_INCLUDE_DIRS )
endmacro()

# {{{1 ocaml_set_link_dirs
# set up link dirs for the target based on the target libs it depends on
macro( ocaml_set_link_dirs target )
    # base value is the target's own build output dir
    set( OCAML_${target}_LINK_DIRS
        "${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/ocaml.${target}.dir"
        )
    foreach( ltarget ${OCAML_${target}_TARGET_DEPLIBS} )
        get_target_property( obj_dir ${ltarget} OBJECT_DIRECTORY )
        list( APPEND OCAML_${target}_LINK_DIRS ${obj_dir} )
    endforeach()
    get_directory_property( link_dirs LINK_DIRECTORIES )
    list( APPEND OCAML_${target}_LINK_DIRS ${link_dirs} )
    list( REMOVE_DUPLICATES OCAML_${target}_LINK_DIRS )
endmacro()

# {{{1 ocaml_get_srcs_lex
macro( ocaml_get_srcs_lex target srcfile srclist )
    get_filename_component( srcfile_we ${srcfile} NAME_WE )
    file( MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/ocaml.${target}.dir )
    execute_process(
        COMMAND ocamllex ${CMAKE_CURRENT_SOURCE_DIR}/${srcfile}
            -o ${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/ocaml.${target}.dir/${srcfile_we}.ml
        OUTPUT_QUIET
        )
    list( APPEND ${srclist} ${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/ocaml.${target}.dir/${srcfile_we}.ml )
endmacro()

# {{{1 ocaml_get_srcs_yacc
macro( ocaml_get_srcs_yacc target srcfile srclist )
    get_filename_component( srcfile_we ${srcfile} NAME_WE )
    file( MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/ocaml.${target}.dir )
    execute_process(
        COMMAND ocamlyacc ${CMAKE_CURRENT_SOURCE_DIR}/${srcfile}
        COMMAND mv
            ${CMAKE_CURRENT_SOURCE_DIR}/${srcfile_we}.mli
            ${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/ocaml.${target}.dir/${srcfile_we}.mli
        COMMAND mv
            ${CMAKE_CURRENT_SOURCE_DIR}/${srcfile_we}.ml
            ${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/ocaml.${target}.dir/${srcfile_we}.ml
        OUTPUT_QUIET
        )
    list( APPEND ${srclist}
        ${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/ocaml.${target}.dir/${srcfile_we}.mli
        ${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/ocaml.${target}.dir/${srcfile_we}.ml
        )
endmacro()

# {{{1 ocaml_set_real_srcs
# collect real source files
macro( ocaml_set_real_srcs target )
    unset( OCAML_${target}_REAL_SRCS )

    foreach( src ${OCAML_${target}_SOURCES} )
        unset( srcfiles )

        get_filename_component( ext ${src} EXT )
        if( ext STREQUAL ".ml" ) # entry is an implementation
            list( APPEND srcfiles ${CMAKE_CURRENT_SOURCE_DIR}/${src} )
        elseif( ext STREQUAL ".mli" ) # entry is an interface
            list( APPEND srcfiles ${CMAKE_CURRENT_SOURCE_DIR}/${src} )
        elseif( ext STREQUAL ".mll" ) # entry is a lex file
            ocaml_get_srcs_lex( ${target} ${src} srcfiles )
        elseif( ext STREQUAL ".mly" ) # entry is a yacc file
            ocaml_get_srcs_yacc( ${target} ${src} srcfiles )
        else() # entry is neither, look in file system
            if( EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/${src}.ml" )
                list( APPEND srcfiles "${CMAKE_CURRENT_SOURCE_DIR}/${src}.ml")
            endif()
            if( EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/${src}.mli" )
                list( APPEND srcfiles "${CMAKE_CURRENT_SOURCE_DIR}/${src}.mli")
            endif()
            if( EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/${src}.mll" )
                ocaml_get_srcs_lex( ${target} ${src}.mll srcfiles )
            endif()
            if( EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/${src}.mly" )
                ocaml_get_srcs_yacc( ${target} ${src}.mly srcfiles )
            endif()
            if( NOT srcfiles )
                message( SEND_ERROR "Can't find source files for ${src} (${target}).")
            endif()
        endif()

        foreach( src0 ${srcfiles} )
            if( IS_ABSOLUTE ${src0} )
                list( APPEND OCAML_${target}_REAL_SRCS ${src0} )
            else()
                list( APPEND OCAML_${target}_REAL_SRCS "${CMAKE_CURRENT_SOURCE_DIR}/${src0}" )
            endif()
        endforeach()
    endforeach()
endmacro()

# {{{1 ocaml_get_dependencies
macro( ocaml_get_dependencies target srcfile dependencies )
    unset( ${dependencies} )

    # get som useful variables set up {{{2
    get_filename_component( srcpath ${srcfile} PATH )
    get_filename_component( srcname ${srcfile} NAME )
    get_filename_component( srcname_we ${srcfile} NAME_WE )
    get_filename_component( srcext ${srcfile} EXT )

    set( is_impl TRUE )
    set( dep_arg "-impl" )
    set( has_intf FALSE )
    if( srcext STREQUAL ".mli" )
        set( is_impl FALSE )
        set( dep_arg "-intf" )
    endif()
    if( is_impl AND EXISTS "${srcpath}/${srcname_we}.mli" )
        set( has_intf TRUE )
    endif()
    #message( STATUS " * Dependencies for ${srcfile}" )
    #message( STATUS "   ${srcfile} is_impl: ${is_impl}" )
    #message( STATUS "   ${srcfile} has_intf: ${has_intf}" )
    #message( STATUS "   ${srcfile} dep_arg: ${dep_arg}" )

    # get dependencies {{{2
    execute_process(
        COMMAND ${CMAKE_OCAML_DEP}
            ${OCAML_${target}_OCAMLOPTS}
            ${OCAML_${target}_PF_${srcname}_OCAMLOPTS}
            ${dep_arg} ${srcfile}

        COMMAND cut -d ":" -f 2-

        COMMAND sed -e "s/^ *//g;s/ *$//g"

        COMMAND tr " " ";"

        OUTPUT_VARIABLE dep_mods
        RESULT_VARIABLE res
        OUTPUT_STRIP_TRAILING_WHITESPACE
        )
    #message( STATUS "   ${srcfile} dep_mods: ${dep_mods}" )

    # for each dependency, look for the real file {{{2
    foreach( dep_mod ${dep_mods} )
        unset( location )
        set( dep_has_intf FALSE )

        # convert the first character in the module name to lowercase, to allow
        # comparison with source filename
        execute_process(
            COMMAND echo ${dep_mod}
            # sed on mac does not support \l
            #COMMAND sed -e "s/\\([A-Z]\\)\\(.*\\)/\\l\\1\\2/"
            COMMAND tr "[:upper:]" "[:lower:]"
            OUTPUT_VARIABLE dep_fn
            OUTPUT_STRIP_TRAILING_WHITESPACE
            )
        #message( STATUS " ${srcfile} dep_fn: ${dep_fn}")

        # look among the current target's sources {{{3
        # iterate over the reported sources rather then the list of real
        # sources
        foreach( tgtsrc ${OCAML_${target}_SOURCES} )
            get_filename_component( tgtsrcname ${tgtsrc} NAME_WE ) # drop extension in case
            execute_process(
                COMMAND echo ${tgtsrcname}
                COMMAND tr "[:upper:]" "[:lower:]"
                OUTPUT_VARIABLE ltgtsrcname
                OUTPUT_STRIP_TRAILING_WHITESPACE
            )
            if( dep_fn STREQUAL ${ltgtsrcname} )
                #message( STATUS "   Found ${tgtsrcname} among my own sources." )
                set( location "${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/ocaml.${target}.dir" )
                if( EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/${tgtsrcname}.mli" )
                    set( dep_has_intf TRUE )
                endif()
                break()
            endif()
        endforeach()

        # look among the target's dependencies {{{3
        if( NOT location )
            foreach( tgtdep ${OCAML_${target}_TARGET_DEPLIBS} )
                get_target_property( srcs ${tgtdep} REAL_SRCS )
                foreach( depsrc ${srcs} )
                    get_filename_component( depsrcname ${depsrc} NAME_WE ) # drop extension in case
                    if( dep_fn STREQUAL ${depsrcname} )
                        #message( STATUS "   Found ${depsrcname} among the sources of ${tgtdep}." )
                        get_target_property( tgtdir ${tgtdep} OBJECT_DIRECTORY )
                        set( location "${tgtdir}" )
                        # modify dep_has_intf
                        get_target_property( depsrcdir ${tgtdep} SOURCE_DIRECTORY )
                        if( EXISTS "${depsrcdir}/${depsrcname}.mli" )
                            set( dep_has_intf TRUE )
                        endif()
                        break()
                    endif()
                endforeach()
            endforeach()
        endif()

        # look among the target's include dirs {{{3
        if( NOT location )
            foreach( idir ${OCAML_${target}_INCLUDE_DIRS} )
                if( EXISTS "${idir}/${dep_fn}.cmi" ) # check for .cmx too?
                    set( location ${idir} )
                    set( dep_has_intf TRUE )
                elseif( EXISTS "${idir}/${dep_fn}.cmo" ) # check for .cmx too?
                    set( location ${idir} )
                endif()
            endforeach()
        endif()

        # add the dependency based on the findings {{{3
        if( location )
            #message( STATUS "Should add the dependencies for ${dep} in ${location} (intf: ${dep_has_intf})" )
            if( dep_has_intf )
                list( APPEND ${dependencies}
                    "${location}/${dep_fn}.cmi"
                    )
            else()
                list( APPEND ${dependencies}
                    "${location}/${dep_fn}.cmo"
                    "${location}/${dep_fn}.cmx"
                    )
            endif()
        else()
            #message( STATUS "Can't find the location of the dependency '${dep}' for '${target}' (${srcfile})" )
        endif()
    endforeach()

    # add a dependency on the sourcefile's .cmi, if there's a .mli file {{{2
    if( is_impl AND has_intf )
        list( APPEND ${dependencies} "${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/ocaml.${target}.dir/${srcname_we}.cmi" )
    endif()

endmacro()


# {{{1 ocaml_add_impl_obj
# adds targets for an OCaml source file, compiling it to both native and
# bytecode
macro( ocaml_add_impl_obj target srcfile )
    get_filename_component( path ${srcfile} PATH )
    get_filename_component( name ${srcfile} NAME )
    get_filename_component( name_we ${srcfile} NAME_WE )
    get_filename_component( ext ${srcfile} EXT )

    set( cmo_name "${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/ocaml.${target}.dir/${name_we}.cmo" )
    set( cmx_name "${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/ocaml.${target}.dir/${name_we}.cmx" )
    set( obj_name "${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/ocaml.${target}.dir/${name_we}.o" )
    set( annot_name "${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/ocaml.${target}.dir/${name_we}.annot" )
    set( cmi_name "" )
    file( RELATIVE_PATH annot_rel ${CMAKE_CURRENT_SOURCE_DIR} ${annot_name} )

    # set up list of outputs
    set( output ${cmo_name} ${cmx_name} ${obj_name} )
    if( NOT EXISTS "${path}/${name_we}.mli" )
        set( cmi_name "${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/ocaml.${target}.dir/${name_we}.cmi" )
        list( APPEND output ${cmi_name} )
    endif()
    #message( STATUS "   ${srcfile} output: ${output}" )

    # include flags
    unset( include_flags )
    foreach( idir ${OCAML_${target}_INCLUDE_DIRS} )
        list( APPEND include_flags -I ${idir} )
    endforeach()
    #message( STATUS "   ${srcfile} include_flags: ${include_flags}" )

    # package flags
    unset( package_flags )
    foreach( pkg ${OCAML_${target}_TARGET_TRANSPKGS} )
        list( APPEND package_flags -package ${pkg} )
    endforeach()
    #message( STATUS "   ${srcfile} package_flags: ${package_flags}" )

    # calculate dependencies
    ocaml_get_dependencies( ${target} ${srcfile} depends )
    #message( STATUS "   ${srcfile} depends: ${depends}" )

    add_custom_command( OUTPUT ${output}
        COMMAND ${CMAKE_OCAML_COMPILER}
            ${include_flags}
            ${package_flags}
            ${OCAML_${target}_OCAMLOPTS}
            ${OCAML_${target}_OCAMLCOPTS}
            ${OCAML_${target}_PF_${name}_OCAMLOPTS}
            ${OCAML_${target}_PF_${name}_OCAMLCOPTS}
            -o ${cmo_name}
            -c -impl ${srcfile}

        COMMAND ${CMAKE_OCAML_COMPILER_OPT}
            ${include_flags}
            ${package_flags}
            ${OCAML_${target}_OCAMLOPTS}
            ${OCAML_${target}_OCAMLCOPTS}
            ${OCAML_${target}_PF_${name}_OCAMLOPTS}
            ${OCAML_${target}_PF_${name}_OCAMLCOPTS}
            -o ${cmo_name}
            -c -impl ${srcfile}

        COMMAND ${CMAKE_MODULE_PATH}/scripts/maybe_link ${annot_name} ${annot_rel} ${CMAKE_CURRENT_SOURCE_DIR}

        MAIN_DEPENDENCY ${srcfile}
        DEPENDS ${depends}
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMENT "Building OCaml implementation ${name_we}"
        )
    add_custom_target( ${target}.${name_we}.impl
        DEPENDS ${output}
        )
    set_target_properties( ${target}.${name_we}.impl PROPERTIES
        OUTPUT "${output}"
        KIND "MLIMPL"
        CMO "${cmo_name}"
        CMX "${cmx_name}"
        OBJ "${obj_name}"
        CMI "${cmi_name}"
        SOURCE "${srcfile}"
        )
    list( APPEND OCAML_${target}_OBJ_TARGETS "${target}.${name_we}.impl" )
endmacro()

# {{{1 ocaml_add_intf_obj
macro( ocaml_add_intf_obj target srcfile )
    get_filename_component( path ${srcfile} PATH )
    get_filename_component( name ${srcfile} NAME )
    get_filename_component( name_we ${srcfile} NAME_WE )
    get_filename_component( ext ${srcfile} EXT )

    set( output "${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/ocaml.${target}.dir/${name_we}.cmi" )
    #message( STATUS "   ${srcfile} output: ${output}" )

    # include flags
    unset( include_flags )
    foreach( idir ${OCAML_${target}_INCLUDE_DIRS} )
        list( APPEND include_flags -I ${idir} )
    endforeach()
    #message( STATUS "   ${srcfile} include_flags: ${include_flags}" )

    # package flags
    unset( package_flags )
    foreach( pkg ${OCAML_${target}_TARGET_TRANSPKGS} )
        list( APPEND package_flags -package ${pkg} )
    endforeach()
    #message( STATUS "   ${srcfile} package_flags: ${package_flags}" )

    # calculate dependencies
    ocaml_get_dependencies( ${target} ${srcfile} depends )
    message( STATUS "   ${srcfile} depends: ${depends}" )

    add_custom_command( OUTPUT ${output}
        COMMAND ${CMAKE_OCAML_COMPILER}
            ${include_flags}
            ${package_flags}
            ${OCAML_${target}_OCAMLOPTS}
            ${OCAML_${target}_OCAMLCOPTS}
            ${OCAML_${target}_PF_${name}_OCAMLOPTS}
            ${OCAML_${target}_PF_${name}_OCAMLCOPTS}
            -o ${output}
            -c -intf ${srcfile}

        MAIN_DEPENDENCY ${srcfile}
        DEPENDS ${depends}
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMENT "Building OCaml interface ${name_we}"
        )
    add_custom_target( ${target}.${name_we}.intf
        DEPENDS ${output}
        )
    set_target_properties( ${target}.${name_we}.intf PROPERTIES
        OUTPUT "${output}"
        KIND "MLINTF"
        CMO ""
        CMX ""
        OBJ ""
        CMI "${output}"
        SOURCE "${srcfile}"
        )
    list( APPEND OCAML_${target}_OBJ_TARGETS "${target}.${name_we}.intf" )
endmacro()

# {{{1 ocaml_add_objs
macro( ocaml_add_objs target )
    unset( OCAML_${target}_OBJ_TARGETS )
    foreach( srcfile ${OCAML_${target}_REAL_SRCS} )
        get_filename_component( path ${srcfile} PATH )
        get_filename_component( name_we ${srcfile} NAME_WE )
        get_filename_component( ext ${srcfile} EXT )

        if( ext STREQUAL ".ml" )
            ocaml_add_impl_obj( ${target} ${srcfile} )
        elseif( ext STREQUAL ".mli" )
            ocaml_add_intf_obj( ${target} ${srcfile} )
        endif()
    endforeach()
endmacro()

# {{{1 ocaml_add_archives
macro( ocaml_add_archives target )
    # {{{2 collect the cmo, cmx and obj
    unset( cmonames )
    unset( cmxnames )
    unset( objnames )
    foreach( tgtobj ${OCAML_${target}_OBJ_TARGETS} )
        #message( STATUS "   looking up objects for ${tgtobj}" )
        get_target_property( kind ${tgtobj} KIND )
        get_target_property( cmo ${tgtobj} CMO )
        get_target_property( cmx ${tgtobj} CMX )
        get_target_property( obj ${tgtobj} OBJ )
        list( APPEND cmonames ${cmo} )
        list( APPEND cmxnames ${cmx} )
        if( kind STREQUAL "COBJ" )
            list( APPEND objnames ${obj} )
        endif()
    endforeach()
    #message( STATUS "   ${target} archive cmos: ${cmonames}" )
    #message( STATUS "   ${target} archive cmxs: ${cmxnames}" )
    #message( STATUS "   ${target} archive objs: ${objnames}" )

    # {{{2 set up package flags
    unset( package_flags )
    foreach( pkg ${OCAML_${target}_TARGET_TRANSPKGS} )
        list( APPEND package_flags -package ${pkg} )
    endforeach()

    unset( clinkflags )
    unset( carchives )
    foreach( lib ${OCAML_${target}_TARGET_TRANSLIBS} )
        get_target_property( kind ${lib} KIND )
        if( kind STREQUAL "CLIB" )
            #message( STATUS "   clib: ${lib}" )
            get_target_property( objdir ${lib} OBJECT_DIRECTORY )
            list( APPEND clinkflags -ccopt -L${objdir} )
            get_target_property( clibs ${lib} LINK_TARGETS )
            get_target_property( carch ${lib} ARCHIVES )
            list( APPEND carchives ${carch} )
            foreach( clib ${clibs} )
                list( APPEND clinkflags -cclib -l${clib} )
            endforeach()
        endif()
    endforeach()
    #message( STATUS "   clinkflags: ${clinkflags}" )
    #message( STATUS "   carchives: ${carchives}" )

    # {{{2 cma archive
    set( cmaoutput "${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/ocaml.${target}.dir/${target}.cma" )
    add_custom_command( OUTPUT ${cmaoutput}
        COMMAND ${CMAKE_OCAML_COMPILER}
            -a -o ${cmaoutput}
            ${OCAML_${target}_OCAMLOPTS}
            ${OCAML_${target}_OCAMLCOPTS}
            ${package_flags}
            ${objnames}
            ${cmonames}
            ${clinkflags}

        DEPENDS ${objnames} ${cmonames} ${carchives}
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMENT "Building OCaml library archive ${target}.cma"
        )
    add_custom_target( ${target}.cma DEPENDS ${cmaoutput} )
    set_target_properties( ${target}.cma PROPERTIES
        OUTPUT ${cmaoutput}
        OBJS "${cmonames}"
        CMA ${cmaoutput}
        CMXA ""
        )

    # {{{2 cmx archive
    set( cmxoutput "${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/ocaml.${target}.dir/${target}.cmxa" )
    set( liboutput "${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/ocaml.${target}.dir/${target}.a" )
    add_custom_command( OUTPUT ${cmxoutput} ${liboutput}
        COMMAND ${CMAKE_OCAML_COMPILER_OPT}
            -a -o ${cmxoutput}
            ${package_flags}
            ${OCAML_${target}_OCAMLOPTS}
            ${OCAML_${target}_OCAMLCOPTS}
            ${objnames}
            ${cmxnames}
            ${clinkflags}

        DEPENDS ${objnames} ${cmxnames} ${carchives}
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMENT "Building OCaml library archive ${target}.cmxa"
        )
    add_custom_target( ${target}.cmxa DEPENDS ${cmxoutput} )
    set_target_properties( ${target}.cmxa PROPERTIES
        OUTPUT "${cmxoutput};${liboutput}"
        OBJS "${cmxnames}"
        CMA ""
        CMXA "${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/ocaml.${target}.dir/${target}.cmxa"
        )
    add_custom_target( ${target}.a DEPENDS ${liboutput})

    # {{2 .o object file
    set( objoutput "${CMAKE_CURRENT_BINARY_DIR}/${target}.o")
    set( aroutput "${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/ocaml.${target}.dir/lib${target}.a")
    add_custom_command( OUTPUT ${objoutput}
        COMMAND ${CMAKE_OCAML_COMPILER}
            -linkpkg -custom -output-obj -o ${objoutput}
            ${OCAML_${target}_OCAMLOPTS}
            ${OCAML_${target}_OCAMLCOPTS}
            ${package_flags}
            ${objnames}
            ${cmonames}
            ${clinkflags}

        DEPENDS ${objnames} ${cmonames} ${carchives}
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMENT "Building OCaml library object ${target}.o"
        )
    add_custom_target( ${target}.o DEPENDS ${objoutput} )
    set_target_properties( ${target}.o PROPERTIES
        OUTPUT "${objoutput}"
        OBJS "${cmxnames}"
        CMA ""
        CMXA ""
        )
    add_custom_command(OUTPUT ${aroutput}
        COMMAND ar cr ${aroutput} ${objoutput}
        DEPENDS ${objnames} ${cmxnames} ${carchives} ${objoutput}
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMENT "Building OCaml library archive lib${target}.a"
        )
    add_custom_target( lib${target}.a DEPENDS ${aroutput})
    set_target_properties( lib${target}.a PROPERTIES
        OUTPUT "${aroutput}"
        OBJS "${cmxnames}"
        CMA ""
        CMXA ""
        )

    # {{{2 set target's ARCHIVES
    set( OCAML_${target}_ARCHIVES ${target}.cma ${target}.cmxa ${target}.a ${target}.o lib${target}.a)
endmacro()

# {{{1 add_ocaml_library
macro( add_ocaml_library target )
    #message( STATUS "add_ocaml_library( ${target} )" )

    ocaml_parse_arguments( OCAML_${target}
        "SOURCES;LIBRARIES;PACKAGES;OCAMLOPTS;OCAMLCOPTS"
        ""
        ${ARGN}
        )
    #message( STATUS "   OCAML_${target}_SOURCES:          ${OCAML_${target}_SOURCES}" )
    #message( STATUS "   OCAML_${target}_LIBRARIES:        ${OCAML_${target}_LIBRARIES}" )
    #message( STATUS "   OCAML_${target}_PACKAGES:         ${OCAML_${target}_PACKAGES}" )
    #message( STATUS "   OCAML_${target}_OCAMLOPTS:        ${OCAML_${target}_OCAMLOPTS}" )
    #message( STATUS "   OCAML_${target}_OCAMLCOPTS:       ${OCAML_${target}_OCAMLCOPTS}" )

    ocaml_set_target_deplibs( ${target} )
    #message( STATUS "   OCAML_${target}_TARGET_DEPLIBS:   ${OCAML_${target}_TARGET_DEPLIBS}" )

    ocaml_set_target_translibs( ${target} )
    #message( STATUS "   OCAML_${target}_TARGET_TRANSLIBS: ${OCAML_${target}_TARGET_TRANSLIBS}" )

    ocaml_set_target_transpkgs( ${target} )
    #message( STATUS "   OCAML_${target}_TARGET_TRANSPKGS: ${OCAML_${target}_TARGET_TRANSPKGS}" )

    ocaml_set_include_dirs( ${target} )
    #message( STATUS "   OCAML_${target}_INCLUDE_DIRS:     ${OCAML_${target}_INCLUDE_DIRS}" )

    ocaml_set_link_dirs( ${target} )
    #message( STATUS "   OCAML_${target}_LINK_DIRS:        ${OCAML_${target}_LINK_DIRS}" )

    # find the real sources for the target
    ocaml_set_real_srcs( ${target} )
    #message( STATUS "   OCAML_${target}_REAL_SRCS:        ${OCAML_${target}_REAL_SRCS}" )

    # create all the object targets, and collect information needed for the
    # archives
    ocaml_add_objs( ${target} )
    #message( STATUS "   OCAML_${target}_OBJ_TARGETS:      ${OCAML_${target}_OBJ_TARGETS}" )

    # create the target for the archive
    ocaml_add_archives( ${target} )
    #message( STATUS "   OCAML_${target}_ARCHIVES:         ${OCAML_${target}_ARCHIVES}" )

    # create the top-level target for this library and add dependencies on each
    # object target
    add_custom_target( ocaml.${target} ALL )
    set_target_properties( ocaml.${target} PROPERTIES
        SOURCE_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        OBJECT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/ocaml.${target}.dir"
        KIND "LIBRARY"
        SOURCES "${OCAML_${target}_SOURCES}"
        LIBRARIES "${OCAML_${target}_LIBRARIES}"
        PACKAGES "${OCAML_${target}_PACKAGES}"
        OCAMLCOPTS "${OCAML_${target}_OCAMLCOPTS}"
        TARGET_DEPLIBS "${OCAML_${target}_TARGET_DEPLIBS}"
        TARGET_TRANSLIBS "${OCAML_${target}_TARGET_TRANSLIBS}"
        TARGET_TRANSPKGS "${OCAML_${target}_TARGET_TRANSPKGS}"
        INCLUDE_DIRS "${OCAML_${target}_INCLUDE_DIRS}"
        LINK_DIRS "${OCAML_${target}_LINK_DIRS}"
        REAL_SRCS "${OCAML_${target}_REAL_SRCS}"
        OBJ_TARGET "${OCAML_${target}_OBJ_TARGETS}"
        ARCHIVES "${OCAML_${target}_ARCHIVES}"
        )
    add_dependencies( ocaml.${target} ${OCAML_${target}_ARCHIVES} )
endmacro()

# {{{1 ocaml_get_files_from_target
# macro which collects different files from a top-level target
macro( ocaml_get_files_from_target target objs outs srcs )
endmacro()

# {{{1 ocaml_install_library
macro( ocaml_install_library pkg lib )
    #message( STATUS "       package: ${pkg}" )
    #message( STATUS "       library: ${lib}" )
    get_target_property( obj_target ocaml.${lib} OBJ_TARGET )
    #message( STATUS "       obj_target: ${obj_target}" )
    get_target_property( archives ocaml.${lib} ARCHIVES )
    #message( STATUS "       archives: ${archives}" )

    unset( files )
    foreach( tgt ${obj_target} )
        get_target_property( cmi ${tgt} CMI )
        get_target_property( source ${tgt} SOURCE )
        #message( STATUS "       ${tgt} cmi: ${cmi}" )
        #message( STATUS "       ${tgt} source: ${source}" )
        list( APPEND files ${cmi} ${source} )
    endforeach()
    foreach( arch ${archives} )
        get_target_property( out ${arch} OUTPUT )
        #message( STATUS "       ${arch} out: ${out}" )
        list( APPEND files ${out} )
    endforeach()
    #message( STATUS "       files: ${files}" )
    install(
        FILES ${files}
        DESTINATION "${OCAML_INSTALL_${pkg}_DESTINATION}"
        )
endmacro()

# {{{1 ocaml_install_c_library
# ToDo: It seems silly to install the C source files, but maybe that should be
# done since all the ML sources are installed.
macro( ocaml_install_c_library pkg lib )
    #message( STATUS "       package: ${pkg}" )
    #message( STATUS "       library: ${lib}" )
    get_target_property( archives ocaml.${lib} ARCHIVES )
    #message( STATUS "       archives: ${archives}" )
    
    set( files ${archives} )

    install(
        FILES ${files}
        DESTINATION ${OCAML_INSTALL_${pkg}_DESTINATION}
        )
endmacro()

# {{{1 install_ocaml_package
macro( install_ocaml_package pkg )
    #message( STATUS "install_ocaml_package( ${pkg} )" )

    ocaml_parse_arguments( OCAML_INSTALL_${pkg}
        "LIBRARIES;DESTINATION;METAFILE"
        ""
        ${ARGN}
        )
    if( NOT OCAML_INSTALL_${pkg}_DESTINATION )
        set( OCAML_INSTALL_${pkg}_DESTINATION "${CMAKE_OCAML_PKGINSTALLDIR}/${pkg}" )
    endif()
    #message( STATUS "   libraries:   ${OCAML_INSTALL_${pkg}_LIBRARIES}" )
    #message( STATUS "   destination: ${OCAML_INSTALL_${pkg}_DESTINATION}" )
    #message( STATUS "   metafile:    ${OCAML_INSTALL_${pkg}_METAFILE}" )

    foreach( lib ${OCAML_INSTALL_${pkg}_LIBRARIES} )
        get_target_property( kind ocaml.${lib} KIND )
        if( kind STREQUAL "LIBRARY" )
            ocaml_install_library( ${pkg} ${lib} )
        elseif( kind STREQUAL "CLIB" )
            ocaml_install_c_library( ${pkg} ${lib} )
        else()
            #message( SEND_ERROR "Attempting to install non-library ${lib} as a package" )
        endif()
    endforeach()

    if( OCAML_INSTALL_${pkg}_METAFILE )
        install(
            FILES ${OCAML_INSTALL_${pkg}_METAFILE}
            DESTINATION ${OCAML_INSTALL_${pkg}_DESTINATION}
            RENAME "META"
            )
    endif()
endmacro()

# {{{1 ocaml_add_exe
macro( ocaml_add_exe target )
    # {{{2 collect the cmo and cmx
    unset( cmonames )
    unset( cmxnames )
    foreach( tgtobj ${OCAML_${target}_OBJ_TARGETS} )
        #message( STATUS "   looking up objects for ${tgtobj}" )
        get_target_property( cmo ${tgtobj} CMO )
        get_target_property( cmx ${tgtobj} CMX )
        list( APPEND cmonames ${cmo} )
        list( APPEND cmxnames ${cmx} )
    endforeach()
    #message( STATUS "   ${target} archive cmos: ${cmonames}" )
    #message( STATUS "   ${target} archive cmxs: ${cmxnames}" )

    # {{{2 set up package flags
    unset( package_flags )
    foreach( pkg ${OCAML_${target}_TARGET_TRANSPKGS} )
        list( APPEND package_flags -package ${pkg} )
    endforeach()
    #message( STATUS "   ${target} package flags: ${package_flags}" )

    # {{{2 set up include flags
    unset( include_flags )
    foreach( idir ${OCAML_${target}_INCLUDE_DIRS} )
        list( APPEND include_flags -I ${idir} )
    endforeach()
    #message( STATUS "   ${target} include_flags: ${include_flags}" )

    # {{{2 collect cma, cmxa and libs
    unset( cmanames )
    unset( cmatargets )
    unset( cmxanames )
    unset( cmxatargets )
    unset( clibnames )
    unset( clibtargets )
    foreach( deplib ${OCAML_${target}_TARGET_DEPLIBS} )
        #message( STATUS "   looking up libs for ${deplib}" )
        get_target_property( archives ${deplib} ARCHIVES )
        #message( STATUS "   ${deplib} archives: ${archives}" )
        get_target_property( kind ${deplib} KIND )
        #message( STATUS "   ${deplib} kind: ${kind}" )

        if( kind STREQUAL "LIBRARY" )
            foreach( arc ${archives} )
                get_target_property( cma ${arc} CMA )
                get_target_property( cmxa ${arc} CMXA )
                if( cma )
                    #message( STATUS "   ${arc} cma: ${cma}" )
                    list( APPEND cmanames ${cma} )
                    list( APPEND cmatargets ${arc} )
                endif()
                if( cmxa )
                    #message( STATUS "   ${arc} cmxa: ${cmxa}" )
                    list( APPEND cmxanames ${cmxa} )
                    list( APPEND cmxatargets ${arc} )
                endif()
            endforeach()
        elseif( kind STREQUAL "CLIB" )
            list( APPEND libnames ${archives} )
            list( APPEND libtargets ${deplib} )
        endif()

    endforeach()
    #message( STATUS "   cmanames: ${cmanames}" )
    #message( STATUS "   cmxanames: ${cmxanames}" )
    #message( STATUS "   libnames: ${libnames}" )
    #message( STATUS "   cmatargets: ${cmatargets}" )
    #message( STATUS "   cmxatargets: ${cmxatargets}" )
    #message( STATUS "   libtargets: ${libtargets}" )

    unset( cclinkflags )
    foreach( deplib ${OCAML_${target}_TARGET_TRANSLIBS} )
        get_target_property( kind ${deplib} KIND )
        #message( STATUS "*** ${deplib} kind: ${kind}" )
        if( kind STREQUAL "CLIB" )
            get_target_property( objdir ${deplib} OBJECT_DIRECTORY )
            #message( STATUS "*** ${deplib} objdir: ${objdir}" )
            list( APPEND cclinkflags -ccopt -L${objdir} )
        endif()
    endforeach()
    #message( STATUS "   cclinkflags: ${cclinkflags}" )

    # {{{2 bytecode exe
    set( exeoutput "${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/ocaml.${target}.dir/${target}" )
    if( OCAML_${target}_RUN_POST_BUILD )
        add_custom_command( OUTPUT ${exeoutput}
            COMMAND ${CMAKE_OCAML_COMPILER}
                -custom
                -linkpkg
                -o ${exeoutput}
                ${OCAML_${target}_OCAMLCOPTS}
                ${package_flags}
                ${include_flags}
                ${cclinkflags}
                ${cmanames}
                ${cmonames}

            COMMAND ${exeoutput}
            
            DEPENDS ${cmonames} ${cmanames}
            WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
            COMMENT "Building OCaml executable ${target}"
            )
    else()
        add_custom_command( OUTPUT ${exeoutput}
            COMMAND ${CMAKE_OCAML_COMPILER}
                -custom
                -linkpkg
                -o ${exeoutput}
                ${OCAML_${target}_OCAMLCOPTS}
                ${package_flags}
                ${include_flags}
                ${cclinkflags}
                ${cmanames}
                ${cmonames}
            
            DEPENDS ${cmonames} ${cmanames}
            WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
            COMMENT "Building OCaml executable ${target}"
            )
    endif()
    add_custom_target( ${target}.exe DEPENDS ${exeoutput} )
    if( cmatargets )
        add_dependencies( ${target}.exe ${cmatargets} )
    endif()
    set_target_properties( ${target}.exe PROPERTIES
        OUTPUT ${exeoutput}
        )

    # {{{2 optimised exe
    set( optoutput "${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/ocaml.${target}.dir/${target}_opt" )
    if( OCAML_${target}_RUN_POST_BUILD )
        add_custom_command( OUTPUT ${optoutput}
            COMMAND ${CMAKE_OCAML_COMPILER_OPT}
                -o ${optoutput}
                ${OCAML_${target}_OCAMLCOPTS}
                ${package_flags}
                ${include_flags}
                ${cmxanames}
                -linkpkg
                ${cmxnames}

            COMMAND ${optoutput}

            DEPENDS ${cmxnames} ${cmxanames}
            WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
            COMMENT "Building OCaml executable ${target}_opt"
            )
    else()
        add_custom_command( OUTPUT ${optoutput}
            COMMAND ${CMAKE_OCAML_COMPILER_OPT}
                -o ${optoutput}
                ${OCAML_${target}_OCAMLCOPTS}
                ${package_flags}
                ${include_flags}
                ${cmxanames}
                -linkpkg
                ${cmxnames}

            DEPENDS ${cmxnames} ${cmxanames}
            WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
            COMMENT "Building OCaml executable ${target}_opt"
            )
    endif()
    add_custom_target( ${target}.opt DEPENDS ${optoutput} )
    if( cmxatargets )
        add_dependencies( ${target}.opt ${cmxatargets} )
    endif()
    set_target_properties( ${target}.opt PROPERTIES
        OUTPUT ${optoutput}
        )

    # {{{2 set target's EXE
    set( OCAML_${target}_EXE ${target}.exe ${target}.opt )
endmacro()

# {{{1 add_ocaml_executable
macro( add_ocaml_executable target )
    #message( STATUS "add_ocaml_executable( ${target} )" )

    ocaml_parse_arguments( OCAML_${target}
        "SOURCES;LIBRARIES;PACKAGES;OCAMLOPTS;OCAMLCOPTS"
        "RUN_POST_BUILD"
        ${ARGN}
        )
    #message( STATUS "   OCAML_${target}_SOURCES:          ${OCAML_${target}_SOURCES}" )
    #message( STATUS "   OCAML_${target}_LIBRARIES:        ${OCAML_${target}_LIBRARIES}" )
    #message( STATUS "   OCAML_${target}_PACKAGES:         ${OCAML_${target}_PACKAGES}" )
    #message( STATUS "   OCAML_${target}_OCAMLOPTS:        ${OCAML_${target}_OCAMLOPTS}" )
    #message( STATUS "   OCAML_${target}_OCAMLCOPTS:       ${OCAML_${target}_OCAMLCOPTS}" )
    #message( STATUS "   OCAML_${target}_RUN_POST_BUILD:   ${OCAML_${target}_RUN_POST_BUILD}" )

    ocaml_set_target_deplibs( ${target} )
    #message( STATUS "   OCAML_${target}_TARGET_DEPLIBS:   ${OCAML_${target}_TARGET_DEPLIBS}" )

    ocaml_set_target_translibs( ${target} )
    #message( STATUS "   OCAML_${target}_TARGET_TRANSLIBS: ${OCAML_${target}_TARGET_TRANSLIBS}" )

    ocaml_set_target_transpkgs( ${target} )
    #message( STATUS "   OCAML_${target}_TARGET_TRANSPKGS: ${OCAML_${target}_TARGET_TRANSPKGS}" )

    ocaml_set_include_dirs( ${target} )
    #message( STATUS "   OCAML_${target}_INCLUDE_DIRS:     ${OCAML_${target}_INCLUDE_DIRS}" )

    ocaml_set_link_dirs( ${target} )
    #message( STATUS "   OCAML_${target}_LINK_DIRS:        ${OCAML_${target}_LINK_DIRS}" )

    ocaml_set_real_srcs( ${target} )
    #message( STATUS "   OCAML_${target}_REAL_SRCS:        ${OCAML_${target}_REAL_SRCS}" )

    ocaml_add_objs( ${target} )
    #message( STATUS "   OCAML_${target}_OBJ_TARGETS:      ${OCAML_${target}_OBJ_TARGETS}" )

    ocaml_add_exe( ${target} )
    #message( STATUS "   OCAML_${target}_EXE:              ${OCAML_${target}_EXE}" )

    add_custom_target( ocaml.${target} ALL )
    set_target_properties( ocaml.${target} PROPERTIES
        SOURCE_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        OBJECT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/ocaml.${target}.dir"
        KIND "EXECUTABLE"
        SOURCES "${OCAML_${target}_SOURCES}"
        LIBRARIES "${OCAML_${target}_LIBRARIES}"
        PACKAGES "${OCAML_${target}_PACKAGES}"
        OCAMLCOPTS "${OCAML_${target}_OCAMLCOPTS}"
        TARGET_DEPLIBS "${OCAML_${target}_TARGET_DEPLIBS}"
        TARGET_TRANSLIBS "${OCAML_${target}_TARGET_TRANSLIBS}"
        TARGET_TRANSPKGS "${OCAML_${target}_TARGET_TRANSPKGS}"
        INCLUDE_DIRS "${OCAML_${target}_INCLUDE_DIRS}"
        LINK_DIRS "${OCAML_${target}_LINK_DIRS}"
        REAL_SRCS "${OCAML_${target}_REAL_SRCS}"
        OBJ_TARGET "${OCAML_${target}_OBJ_TARGETS}"
        EXE "${OCAML_${target}_EXE}"
        )
    add_dependencies( ocaml.${target} ${OCAML_${target}_OBJ_TARGETS} ${OCAML_${target}_EXE} )
endmacro()

# {{{1 ocaml_add_c_obj
macro( ocaml_add_c_obj target srcfile )
    #message( STATUS "ocaml_add_c_obj( ${target} ${srcfile} )" )
    get_filename_component( path ${srcfile} PATH )
    get_filename_component( name ${srcfile} NAME )
    get_filename_component( name_we ${srcfile} NAME_WE )
    get_filename_component( ext ${srcfile} EXT )

    set( output ${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/ocaml.${target}.dir/${name_we}.o )
    add_custom_command( OUTPUT ${output}
        COMMAND mkdir -p ${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/ocaml.${target}.dir

        COMMAND ${CMAKE_OCAML_COMPILER}
            -ccopt -o${output}
            -c ${srcfile}

        MAIN_DEPENDENCY ${srcfile}
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMENT "Building OCaml C object ${name_we}"
        )
    add_custom_target( ${target}.${name_we}.cobj DEPENDS ${output} )
    set_target_properties( ${target}.${name_we}.cobj PROPERTIES
        OUTPUT "${output}"
        KIND "COBJ"
        CMO ""
        CMX ""
        OBJ "${output}"
        CMI ""
        SOURCE "${srcfile}"
        )
    list( APPEND OCAML_${target}_OBJ_TARGETS "${target}.${name_we}.cobj" )
endmacro()

# {{{1 add_ocaml_c_library
# todo:
#   add support for CCOPTS
#   add support for linking against C libraries
#   re-evaluate whether to use ocamlmklib or straight ar
macro( add_ocaml_c_library target )
    #message( STATUS "add_ocaml_c_library( ${target} )" )

    ocaml_parse_arguments( OCAML_${target}
        "SOURCES;CLIBRARIES"
        ""
        ${ARGN}
        )
    #message( STATUS "   OCAML_${target}_SOURCES:     ${OCAML_${target}_SOURCES}" )
    #message( STATUS "   OCAML_${target}_CLIBRARIES:  ${OCAML_${target}_CLIBRARIES}" )

    unset( OCAML_${target}_OBJ_TARGETS )
    foreach( rs ${OCAML_${target}_SOURCES} )
        ocaml_add_c_obj( ${target} ${CMAKE_CURRENT_SOURCE_DIR}/${rs} )
    endforeach()
    #message( STATUS "   OCAML_${target}_OBJ_TARGETS: ${OCAML_${target}_OBJ_TARGETS}" )

    unset( OCAML_${target}_OBJS )
    foreach( tgt ${OCAML_${target}_OBJ_TARGETS} )
        get_target_property( obj ${tgt} OBJ )
        list( APPEND OCAML_${target}_OBJS ${obj} )
    endforeach()
    #message( STATUS "   OCAML_${target}_OBJS:        ${OCAML_${target}_OBJS}" )

    #set( output_a ${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/ocaml.${target}.dir/lib${target}.a )
    #set( output_dll ${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/ocaml.${target}.dir/dll${target}.so )
    #set( output "${output_a};${output_dll}" )
    #add_custom_command( OUTPUT ${output}
    #    COMMAND ocamlmklib -oc ${target}
    #        ${OCAML_${target}_OBJS}
    #    DEPENDS ${OCAML_${target}_OBJS}
    #    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/ocaml.${target}.dir
    #    COMMENT "Building OCaml C lib ${target}"
    #    )
    #add_custom_target( ocaml.${target} ALL DEPENDS ${output} )
    #set_target_properties( ocaml.${target} PROPERTIES
    #    KIND "CLIB"
    #    LINK_TARGETS "${target};${OCAML_${target}_CLIBRARIES}"
    #    SOURCE_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    #    OBJECT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/ocaml.${target}.dir
    #    LIBRARIES ""
    #    PACKAGES ""
    #    OCAMLCOPTS ""
    #    TARGET_DEPLIBS ""
    #    TARGET_TRANSLIBS ""
    #    TARGET_TRANSPKGS ""
    #    INCLUDE_DIRS ""
    #    LINK_DIRS ""
    #    REAL_SRCS ""
    #    OBJ_TARGET "${OCAML_${target}_OBJ_TARGETS}"
    #    ARCHIVES "${output}"
    #    )

    set( output_a ${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/ocaml.${target}.dir/lib${target}.a )
    set( output "${output_a}" )
    add_custom_command( OUTPUT ${output}
        COMMAND ar rc ${output}
            ${OCAML_${target}_OBJS}
        DEPENDS ${OCAML_${target}_OBJS}
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMENT "Building OCaml C lib lib${target}.a"
        )
    add_custom_target( ocaml.${target} ALL DEPENDS ${output} )
    set_target_properties( ocaml.${target} PROPERTIES
        KIND "CLIB"
        LINK_TARGETS "${target};${OCAML_${target}_CLIBRARIES}"
        SOURCE_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        OBJECT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/ocaml.${target}.dir
        LIBRARIES ""
        PACKAGES ""
        OCAMLCOPTS ""
        TARGET_DEPLIBS ""
        TARGET_TRANSLIBS ""
        TARGET_TRANSPKGS ""
        INCLUDE_DIRS ""
        LINK_DIRS ""
        REAL_SRCS ""
        OBJ_TARGET "${OCAML_${target}_OBJ_TARGETS}"
        ARCHIVES "${output}"
        )
endmacro()

# {{{1 install_ocaml_executable
macro( install_ocaml_executable exe )
    #message( STATUS "install_ocaml_executable( ${exe} )" )

    ocaml_parse_arguments( OCAML_INSTALL_${exe}
        "DESTINATION"
        ""
        ${ARGN}
        )
    if( NOT OCAML_INSTALL_${exe}_DESTINATION )
        set( OCAML_INSTALL_${exe}_DESTINATION "bin" )
    endif()
    #message( STATUS "   destination: ${OCAML_INSTALL_${exe}_DESTINATION}" )

    get_target_property( tgts ocaml.${exe} EXE )
    #message( STATUS "   tgts: ${tgts}" )
    unset( progs )
    foreach( tgt ${tgts} )
        get_target_property( out ${tgt} OUTPUT )
        #message( STATUS "   ${tgt} output: ${out}" )
        list( APPEND progs ${out} )
    endforeach()
    install( PROGRAMS ${progs}
        DESTINATION ${OCAML_INSTALL_${exe}_DESTINATION}
        )
endmacro()

# {{{1 add_ocaml_file_options
macro( add_ocaml_file_options target )
    #message( STATUS "add_ocaml_file_options( ${target} )" )
    ocaml_parse_arguments( OCAML_${target}_PF
        "SOURCES;OCAMLOPTS;OCAMLCOPTS"
        ""
        ${ARGN}
        )
    message( STATUS "   sources: ${OCAML_${target}_PF_SOURCES}" )
    message( STATUS "   ocamlopts: ${OCAML_${target}_PF_OCAMLOPTS}" )
    message( STATUS "   ocamlopts: ${OCAML_${target}_PF_OCAMLCOPTS}" )

    foreach( src ${OCAML_${target}_PF_SOURCES} )
        set( OCAML_${target}_PF_${src}_OCAMLOPTS ${OCAML_${target}_PF_OCAMLOPTS} )
        set( OCAML_${target}_PF_${src}_OCAMLCOPTS ${OCAML_${target}_PF_OCAMLCOPTS} )
        message( STATUS "   ${src} opts: ${OCAML_${target}_PF_${src}_OCAMLOPTS}" )
        message( STATUS "   ${src} opts: ${OCAML_${target}_PF_${src}_OCAMLCOPTS}" )
    endforeach()
endmacro()

# vim: set tw=0 :
