CLASS ycl_simbal_gui DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF exception_dict,
             error TYPE REF TO cx_root,
           END OF exception_dict,

           exception_list TYPE STANDARD TABLE OF exception_dict WITH EMPTY KEY.

    CLASS-METHODS display_cx_msg_popup
      IMPORTING
        !cx   TYPE REF TO cx_root
        !text TYPE clike OPTIONAL .

    CLASS-METHODS display_cx_msgs_popup
      IMPORTING
        !cxs  TYPE exception_list
        !text TYPE clike OPTIONAL .

    CLASS-METHODS display_cx_msg_matryoshka IMPORTING !cx TYPE REF TO cx_root .
    CLASS-METHODS display_cx_msg_i IMPORTING !cx TYPE REF TO cx_root .

    METHODS constructor IMPORTING !simbal TYPE REF TO ycl_simbal.

    METHODS show_light_popup
      EXPORTING !exit_command TYPE bal_s_excm
      RAISING   ycx_simbal_log.

    METHODS show_single
      EXPORTING !exit_command TYPE bal_s_excm
      RAISING   ycx_simbal_log.

    METHODS show_popup
      EXPORTING !exit_command TYPE bal_s_excm
      RAISING   ycx_simbal_log.

    METHODS show_standard
      EXPORTING !exit_command TYPE bal_s_excm
      RAISING   ycx_simbal_log.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA dummy_balsub TYPE balsub.
    DATA simbal TYPE REF TO ycl_simbal.

    CLASS-METHODS get_dummy_balsub RETURNING VALUE(output) TYPE balsub.

    METHODS show_with_bal_profile
      IMPORTING !profile      TYPE bal_s_prof
      EXPORTING !exit_command TYPE bal_s_excm
      RAISING   ycx_simbal_log.
ENDCLASS.



CLASS ycl_simbal_gui IMPLEMENTATION.
  METHOD display_cx_msg_popup.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Shows an exception message in a SIMBAL popup
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    TRY.
        DATA(balsub) = get_dummy_balsub( ).

        DATA(simbal) = NEW ycl_simbal(
            object    = balsub-object
            subobject = balsub-subobject ).

        IF text IS SUPPLIED.
          simbal->add_free_text( text  = text
                                 msgty = ycl_simbal=>msgty-error ).
        ENDIF.

        simbal->add_exception( cx ).
        NEW ycl_simbal_gui( simbal )->show_light_popup( ).

      CATCH cx_root.
        display_cx_msg_matryoshka( cx ).

        IF text IS SUPPLIED.
          MESSAGE text TYPE ycl_simbal=>msgty-info.
        ENDIF.
    ENDTRY.
  ENDMETHOD.


  METHOD display_cx_msgs_popup.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Shows a list of exception message in a SIMBAL popup
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CHECK cxs IS NOT INITIAL.

    TRY.
        DATA(balsub) = get_dummy_balsub( ).

        DATA(simbal) = NEW ycl_simbal(
            object    = balsub-object
            subobject = balsub-subobject ).

        IF text IS SUPPLIED.
          simbal->add_free_text( text  = text
                                 msgty = ycl_simbal=>msgty-error ).
        ENDIF.

        LOOP AT cxs ASSIGNING FIELD-SYMBOL(<cx>) WHERE error IS NOT INITIAL.
          simbal->add_exception( <cx>-error ).
        ENDLOOP.

        NEW ycl_simbal_gui( simbal )->show_light_popup( ).

      CATCH cx_root.
        LOOP AT cxs ASSIGNING <cx> WHERE error IS NOT INITIAL.
          display_cx_msg_matryoshka( <cx>-error ).
        ENDLOOP.

        IF text IS SUPPLIED.
          MESSAGE text TYPE ycl_simbal=>msgty-info.
        ENDIF.
    ENDTRY.
  ENDMETHOD.


  METHOD display_cx_msg_matryoshka.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Displays the given exception as popups
    " from the deepest to the shallowest
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CHECK cx IS NOT INITIAL.
    display_cx_msg_matryoshka( cx->previous ).
    display_cx_msg_i( cx ).
  ENDMETHOD.


  METHOD display_cx_msg_i.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Displays the given exception as an information box
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CHECK cx IS NOT INITIAL.
    MESSAGE cx TYPE ycl_simbal=>msgty-info.
  ENDMETHOD.


  METHOD constructor.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Object creation
    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    me->simbal = simbal.
  ENDMETHOD.


  METHOD show_light_popup.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Displays messages in light popup mode
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CLEAR exit_command.

    IF me->simbal IS INITIAL.
      RETURN.
    ENDIF.

    DATA(messages_to_show) = me->simbal->get_messages( ).
    IF lines( messages_to_show ) <= 0.
      RETURN.
    ENDIF.

    CALL FUNCTION 'MESSAGES_INITIALIZE'.
    DATA(lineno) = CONV msgzeile( 0 ).

    LOOP AT messages_to_show ASSIGNING FIELD-SYMBOL(<msg>).
      lineno = lineno + 1.

      CALL FUNCTION 'MESSAGE_STORE'
        EXPORTING
          arbgb = <msg>-id
          msgty = <msg>-type
          msgv1 = <msg>-message_v1
          msgv2 = <msg>-message_v2
          msgv3 = <msg>-message_v3
          msgv4 = <msg>-message_v4
          txtnr = <msg>-number
          zeile = lineno.
    ENDLOOP.

    CALL FUNCTION 'MESSAGES_STOP'
      EXCEPTIONS
        a_message = 1
        e_message = 2
        i_message = 3
        w_message = 4 ##FM_SUBRC_OK.

    CALL FUNCTION 'MESSAGES_SHOW'
      EXCEPTIONS
        OTHERS = 99 ##FM_SUBRC_OK ##NUMBER_OK.

    IMPORT e_exit_command TO exit_command FROM MEMORY ID 'E_EXIT_COMMAND'.
  ENDMETHOD.


  METHOD show_single.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Shows in BAL single mode
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CLEAR exit_command.
    DATA(profile) = VALUE bal_s_prof( ).

    CALL FUNCTION 'BAL_DSP_PROFILE_SINGLE_LOG_GET'
      IMPORTING
        e_s_display_profile = profile.

    show_with_bal_profile( profile ).
  ENDMETHOD.


  METHOD show_popup.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Shows in BAL popup mode
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CLEAR exit_command.
    DATA(profile) = VALUE bal_s_prof( ).

    CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
      IMPORTING
        e_s_display_profile = profile.

    show_with_bal_profile( profile ).
  ENDMETHOD.


  METHOD show_standard.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Shows in BAL standard mode
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CLEAR exit_command.
    DATA(profile) = VALUE bal_s_prof( ).

    CALL FUNCTION 'BAL_DSP_PROFILE_STANDARD_GET'
      IMPORTING
        e_s_display_profile = profile.

    show_with_bal_profile( profile ).
  ENDMETHOD.


  METHOD get_dummy_balsub.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns a dummy BALSUB key for display purposes only
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF ycl_simbal_gui=>dummy_balsub IS INITIAL.
      SELECT SINGLE * FROM balsub                       "#EC CI_GENBUFF
             WHERE object LIKE 'Y%' OR                  "#EC CI_NOORDER
                   object LIKE 'Z%'
             INTO CORRESPONDING FIELDS OF
             @ycl_simbal_gui=>dummy_balsub.

      IF sy-subrc <> 0.
        SELECT SINGLE * FROM balsub                     "#EC CI_GENBUFF
               INTO CORRESPONDING FIELDS OF             "#EC CI_NOORDER
               @ycl_simbal_gui=>dummy_balsub.
      ENDIF.
    ENDIF.

    output = ycl_simbal_gui=>dummy_balsub.
  ENDMETHOD.


  METHOD show_with_bal_profile.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Shows messages with the given BAL profile
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CLEAR exit_command.

    IF me->simbal IS INITIAL.
      RETURN.
    ENDIF.

    DATA(prof) = profile.

    prof-use_grid = abap_true.
    prof-title = TEXT-001.
    prof-tree_ontop = abap_false.
    prof-head_text = TEXT-001.
    prof-head_size = 35.                                 "#EC NUMBER_OK
    prof-tree_size = 22.                                 "#EC NUMBER_OK
    prof-show_all = abap_true.
    prof-disvariant-report = sy-repid.

    prof-lev1_fcat = VALUE #(
        ref_table = 'BAL_S_SHOW'
        ( ref_field = 'LOG_HANDLE'
          col_pos   = 0
          no_out    = abap_true )
        ( ref_field = 'EXTNUMBER'
          col_pos   = 1
          outputlen = 40 )

        ( ref_field  = 'ALDATE'
          col_pos    = 2
          colddictxt = 'R'
          is_treecol = abap_true )

        ( ref_field  = 'ALTIME'
          col_pos    = 3
          colddictxt = 'R'
          is_treecol = abap_true )

        ( ref_field  = 'ALUSER'
          col_pos    = 4
          colddictxt = 'R'
          is_treecol = abap_true ) ) ##NUMBER_OK.

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_s_display_profile  = prof
      IMPORTING
        e_s_exit_command     = exit_command
      EXCEPTIONS
        profile_inconsistent = 1
        internal_error       = 2
        no_data_available    = 3
        no_authority         = 4
        OTHERS               = 5.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ycx_simbal_log
        EXPORTING
          textid    = ycx_simbal_log=>cant_display
          object    = me->simbal->object
          subobject = me->simbal->subobject.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
