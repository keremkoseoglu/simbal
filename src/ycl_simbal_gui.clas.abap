CLASS ycl_simbal_gui DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
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
    DATA simbal TYPE REF TO ycl_simbal.

    METHODS show_with_bal_profile
      IMPORTING !profile      TYPE bal_s_prof
      EXPORTING !exit_command TYPE bal_s_excm
      RAISING   ycx_simbal_log.
ENDCLASS.



CLASS ycl_simbal_gui IMPLEMENTATION.
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

    CALL FUNCTION 'MESSAGES_INITIALIZE'.
    DATA(lineno) = CONV msgzeile( 0 ).

    LOOP AT me->simbal->get_messages( ) ASSIGNING FIELD-SYMBOL(<msg>).
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
        others = 99 ##NUMBER_OK ##FM_SUBRC_OK.

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


  METHOD show_with_bal_profile.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Shows messages with the given BAL profile
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CLEAR exit_command.
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
