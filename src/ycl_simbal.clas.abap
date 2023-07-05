CLASS ycl_simbal DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES bapi_coru_return_list TYPE STANDARD TABLE OF bapi_coru_return WITH EMPTY KEY.
    TYPES bapireturn_list TYPE STANDARD TABLE OF bapireturn WITH EMPTY KEY.
    TYPES bapireturn1_list TYPE STANDARD TABLE OF bapireturn1 WITH EMPTY KEY.
    TYPES severity_type TYPE char1.
    TYPES mesg_list TYPE STANDARD TABLE OF mesg WITH EMPTY KEY.
    TYPES message_type_range TYPE RANGE OF symsgty.

    CONSTANTS default_preservation TYPE i VALUE 60.

    CONSTANTS: BEGIN OF msgty,
                 abort   TYPE symsgty VALUE 'A',
                 error   TYPE symsgty VALUE 'E',
                 info    TYPE symsgty VALUE 'I',
                 status  TYPE symsgty VALUE 'S',
                 warning TYPE symsgty VALUE 'W',
                 exit    TYPE symsgty VALUE 'X',
               END OF msgty.

    CONSTANTS: BEGIN OF severity,
                 all     TYPE severity_type VALUE space,
                 error   TYPE severity_type VALUE 'E',
                 lock    TYPE severity_type VALUE 'L',
                 warning TYPE severity_type VALUE 'W',
               END OF severity.

    DATA object TYPE balobj_d READ-ONLY.
    DATA subobject TYPE balsubobj READ-ONLY.
    DATA log_handle TYPE balloghndl READ-ONLY.

    CLASS-METHODS get_crit_msgty_range
      RETURNING
        VALUE(output) TYPE message_type_range.

    METHODS constructor
      IMPORTING
        !object            TYPE balobj_d
        !subobject         TYPE balsubobj
        !preservation_days TYPE int4 DEFAULT default_preservation
        !extnumber         TYPE balnrext OPTIONAL
      RAISING
        ycx_simbal_log .

    METHODS add_bapi_coru_return
      IMPORTING
        !bapi_coru_return TYPE bapi_coru_return_list.

    METHODS add_bapiret1
      IMPORTING
        !bapireturn1 TYPE bapiret1_list .

    METHODS add_bapiret2
      IMPORTING
        !bapiret2 TYPE bapiret2_tab
        !cumulate TYPE abap_bool DEFAULT abap_false .

    METHODS add_bapireturn
      IMPORTING
        !bapireturn TYPE bapireturn_list.

    METHODS add_bapireturn1
      IMPORTING
        !bapireturn1 TYPE bapireturn1_list .

    METHODS add_bdcmsgcoll
      IMPORTING
        !bdcmsgcoll TYPE ettcd_msg_tabtype .

    METHODS add_bcsy_text
      IMPORTING
        !bcsy_text TYPE bcsy_text
        !msgty     TYPE symsgty DEFAULT msgty-status .

    METHODS add_deepest_exception
      IMPORTING
        !cx    TYPE REF TO cx_root
        !msgty TYPE symsgty DEFAULT msgty-error .

    METHODS add_exception
      IMPORTING
        !cx    TYPE REF TO cx_root
        !msgty TYPE symsgty DEFAULT msgty-error .

    METHODS add_free_text
      IMPORTING
        !text  TYPE c
        !msgty TYPE symsgty DEFAULT msgty-status .

    METHODS add_itab_fld_as_free_text
      IMPORTING
        !tab   TYPE REF TO data
        !fld   TYPE fieldname
        !intro TYPE clike OPTIONAL
        !msgty TYPE symsgty DEFAULT msgty-status
      RAISING
        ycx_simbal_log.

    METHODS add_mesg IMPORTING !mesg TYPE mesg_list.

    METHODS add_string
      IMPORTING
        !msg   TYPE string
        !msgty TYPE symsgty DEFAULT msgty-error.

    METHODS add_swr
      IMPORTING
        !swr TYPE swr_msgtab .

    METHODS add_swr_messag
      IMPORTING
        !swr TYPE sapi_msg_lines .

    METHODS add_sy_msg
      IMPORTING
        !cumulate TYPE abap_bool DEFAULT abap_false .

    METHODS add_t100_msg
      IMPORTING
        !msgid    TYPE symsgid
        !msgno    TYPE symsgno
        !msgty    TYPE symsgty
        !msgv1    TYPE data OPTIONAL
        !msgv2    TYPE data OPTIONAL
        !msgv3    TYPE data OPTIONAL
        !msgv4    TYPE data OPTIONAL
        !cumulate TYPE abap_bool DEFAULT abap_false .

    METHODS clear_log .

    METHODS get_message_count
      IMPORTING
        !msgty_x      TYPE flag DEFAULT abap_true
        !msgty_a      TYPE flag DEFAULT abap_true
        !msgty_e      TYPE flag DEFAULT abap_true
        !msgty_w      TYPE flag DEFAULT abap_true
        !msgty_i      TYPE flag DEFAULT abap_true
        !msgty_s      TYPE flag DEFAULT abap_true
      RETURNING
        VALUE(output) TYPE int4 .

    METHODS get_messages
      IMPORTING
        !msgty_x      TYPE abap_bool DEFAULT abap_true
        !msgty_a      TYPE abap_bool DEFAULT abap_true
        !msgty_e      TYPE abap_bool DEFAULT abap_true
        !msgty_w      TYPE abap_bool DEFAULT abap_true
        !msgty_i      TYPE abap_bool DEFAULT abap_true
        !msgty_s      TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(output) TYPE bapiret2_t .

    METHODS get_messages_as_json
      IMPORTING
        !msgty_x      TYPE abap_bool DEFAULT abap_true
        !msgty_a      TYPE abap_bool DEFAULT abap_true
        !msgty_e      TYPE abap_bool DEFAULT abap_true
        !msgty_w      TYPE abap_bool DEFAULT abap_true
        !msgty_i      TYPE abap_bool DEFAULT abap_true
        !msgty_s      TYPE abap_bool DEFAULT abap_true
        !short        TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(output) TYPE string.

    METHODS get_text_of_msgid
      IMPORTING
        VALUE(cl)     TYPE sy-msgid
        VALUE(number) TYPE sy-msgno
      RETURNING
        VALUE(output) TYPE bapi_msg .

    METHODS get_worst_severity
      RETURNING
        VALUE(output) TYPE severity_type .

    METHODS save_to_db_2nd_connection
      CHANGING
        VALUE(save_all) TYPE abap_bool DEFAULT abap_true
      RAISING
        ycx_simbal_log .

    METHODS save_to_db
      IMPORTING
        !commit         TYPE abap_bool DEFAULT abap_false
      CHANGING
        VALUE(save_all) TYPE abap_bool DEFAULT abap_true
      RAISING
        ycx_simbal_log .

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS: BEGIN OF option,
                 eq TYPE ddoption VALUE 'EQ',
               END OF option.

    CONSTANTS: BEGIN OF sign,
                 include TYPE ddsign VALUE 'I',
               END OF sign.

    CONSTANTS: BEGIN OF msgid,
                 simbal TYPE symsgid VALUE 'YSIMBAL',
               END OF msgid.

    CLASS-DATA critical_message_type_rng TYPE message_type_range.

    CLASS-METHODS determine_pclass
      IMPORTING
        !msgty        TYPE symsgty
      RETURNING
        VALUE(output) TYPE bal_s_msg-probclass .

    CLASS-METHODS get_json_text
      IMPORTING !val          TYPE any
      RETURNING VALUE(output) TYPE string.

ENDCLASS.



CLASS YCL_SIMBAL IMPLEMENTATION.


  METHOD add_bapiret1.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Adds a list of BAPIRET1 messages to the log
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    LOOP AT bapireturn1 ASSIGNING FIELD-SYMBOL(<br1>).
      add_t100_msg( msgid    = <br1>-id
                    msgno    = <br1>-number
                    msgty    = <br1>-type
                    msgv1    = <br1>-message_v1
                    msgv2    = <br1>-message_v2
                    msgv3    = <br1>-message_v3
                    msgv4    = <br1>-message_v4
                    cumulate = abap_false ).
    ENDLOOP.
  ENDMETHOD.


  METHOD add_bapiret2.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Adds a list of BAPIRET2 messages to the log
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    LOOP AT bapiret2 ASSIGNING FIELD-SYMBOL(<br2>).
      IF <br2>-id IS INITIAL AND <br2>-message IS NOT INITIAL.
        add_free_text(
            text  = <br2>-message
            msgty = COND #( WHEN <br2>-type IS NOT INITIAL
                            THEN <br2>-type
                            ELSE me->msgty-status ) ).
      ELSE.
        add_t100_msg( msgid    = <br2>-id
                      msgno    = <br2>-number
                      msgty    = <br2>-type
                      msgv1    = <br2>-message_v1
                      msgv2    = <br2>-message_v2
                      msgv3    = <br2>-message_v3
                      msgv4    = <br2>-message_v4
                      cumulate = cumulate ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD add_bapireturn.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Adds a list of BAPIRETURN messages to the log
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    LOOP AT bapireturn ASSIGNING FIELD-SYMBOL(<br>).
      add_free_text( text  = <br>-message
                     msgty = <br>-type ).
    ENDLOOP.
  ENDMETHOD.


  METHOD add_bapireturn1.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Adds a list of BAPIRETURN1 messages to the log
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    LOOP AT bapireturn1 ASSIGNING FIELD-SYMBOL(<br1>).
      add_t100_msg( msgid    = <br1>-id
                    msgno    = <br1>-number
                    msgty    = <br1>-type
                    msgv1    = <br1>-message_v1
                    msgv2    = <br1>-message_v2
                    msgv3    = <br1>-message_v3
                    msgv4    = <br1>-message_v4
                    cumulate = abap_false ).
    ENDLOOP.
  ENDMETHOD.


  METHOD add_bapi_coru_return.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Adds a list of BAPI_CORU_RETURN messages to the log
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    LOOP AT bapi_coru_return ASSIGNING FIELD-SYMBOL(<bcr>).
      add_t100_msg( msgid    = <bcr>-id
                    msgno    = <bcr>-number
                    msgty    = <bcr>-type
                    msgv1    = <bcr>-message_v1
                    msgv2    = <bcr>-message_v2
                    msgv3    = <bcr>-message_v3
                    msgv4    = <bcr>-message_v4
                    cumulate = abap_false ).
    ENDLOOP.
  ENDMETHOD.


  METHOD add_bcsy_text.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Adds a list of BCSY_TEXT messages to the log
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    LOOP AT bcsy_text ASSIGNING FIELD-SYMBOL(<bt>).
      add_free_text( text  = <bt>-line
                     msgty = msgty ).
    ENDLOOP.
  ENDMETHOD.


  METHOD add_bdcmsgcoll.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Adds a list of BDCMSGCOLL messages to the log
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    LOOP AT bdcmsgcoll ASSIGNING FIELD-SYMBOL(<bmc>).
      add_t100_msg( msgid    = <bmc>-msgid
                    msgno    = CONV #( <bmc>-msgnr )
                    msgty    = <bmc>-msgtyp
                    msgv1    = <bmc>-msgv1
                    msgv2    = <bmc>-msgv2
                    msgv3    = <bmc>-msgv3
                    msgv4    = <bmc>-msgv4
                    cumulate = abap_false ).
    ENDLOOP.
  ENDMETHOD.


  METHOD add_deepest_exception.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Adds the deepest PREVIOUS exception to the log
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CHECK cx IS NOT INITIAL.

    IF cx->previous IS INITIAL.
      add_exception( cx    = cx
                     msgty = msgty ).
    ELSE.
      add_deepest_exception( cx    = cx->previous
                             msgty = msgty ).
    ENDIF.
  ENDMETHOD.


  METHOD add_exception.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Adds an exception to the log
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CHECK cx IS NOT INITIAL.

    add_exception( cx    = cx->previous
                   msgty = msgty ).

    CASE cx->is_resumable.
      WHEN abap_true.
        add_free_text( text  = CONV text200( cx->get_text( ) )
                       msgty = msgty ).

      WHEN abap_false.
        MESSAGE cx TYPE me->msgty-status.
        sy-msgty = msgty.
        add_sy_msg( ).
    ENDCASE.
  ENDMETHOD.


  METHOD add_free_text.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Adds any text to the log
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
      EXPORTING
        i_log_handle  = me->log_handle
        i_text        = text
        i_msgty       = msgty
        i_probclass   = determine_pclass( msgty )
      EXCEPTIONS
        log_not_found = 0
        OTHERS        = 1 ##FM_SUBRC_OK.
  ENDMETHOD.


  METHOD add_itab_fld_as_free_text.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Adds the given field of an ITAB to the log
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    FIELD-SYMBOLS <itab> TYPE ANY TABLE.

    TRY.
        IF intro IS NOT INITIAL.
          add_free_text( text  = intro
                         msgty = msgty ).
        ENDIF.

        IF tab IS INITIAL.
          RETURN.
        ENDIF.

        ASSIGN tab->* TO <itab>.

        IF NOT ( <itab> IS ASSIGNED AND
                 <itab> IS NOT INITIAL ).
          RETURN.
        ENDIF.

        LOOP AT <itab> ASSIGNING FIELD-SYMBOL(<entry>).
          ASSIGN COMPONENT fld
                 OF STRUCTURE <entry>
                 TO FIELD-SYMBOL(<value>).

          add_free_text( text  = <value>
                         msgty = msgty ).
        ENDLOOP.

      CATCH cx_root INTO DATA(diaper).
        RAISE EXCEPTION TYPE ycx_simbal_log
          EXPORTING
            textid   = ycx_simbal_log=>add_message_error
            previous = diaper.
    ENDTRY.
  ENDMETHOD.


  METHOD add_mesg.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Adds a list of MESG messages to the log
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    LOOP AT mesg ASSIGNING FIELD-SYMBOL(<mesg>).
      add_free_text( text  = <mesg>-text
                     msgty = <mesg>-msgty ).
    ENDLOOP.
  ENDMETHOD.


  METHOD add_string.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Adds a string to the log
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA br2 TYPE bapiret2_tab.
    DATA ret TYPE TABLE OF char50.
    DATA cmsg(9999).

    cmsg = msg.

    CALL FUNCTION 'IQAPI_WORD_WRAP'
      EXPORTING
        textline            = cmsg
        outputlen           = 50
      TABLES
        out_lines           = ret
      EXCEPTIONS
        outputlen_too_large = 1
        OTHERS              = 2. "#EC NUMBER_OK

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    APPEND INITIAL LINE TO br2 ASSIGNING FIELD-SYMBOL(<return>).

    LOOP AT ret ASSIGNING FIELD-SYMBOL(<ret>).
      <return>-id = me->msgid-simbal.
      <return>-number = '006'.
      <return>-type = msgty.
      IF  <return>-message_v1 IS INITIAL.
        <return>-message_v1 = <ret>.
      ELSEIF  <return>-message_v2 IS INITIAL.
        <return>-message_v2 = <ret>.
      ELSEIF  <return>-message_v3 IS INITIAL.
        <return>-message_v3 = <ret>.
      ELSEIF  <return>-message_v4 IS INITIAL.
        <return>-message_v4 = <ret>.
        APPEND INITIAL LINE TO br2 ASSIGNING <return>.
      ENDIF.
    ENDLOOP.

    DELETE br2 WHERE message_v1 IS INITIAL.
    add_bapiret2( br2 ).
  ENDMETHOD.


  METHOD add_swr.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Adds a list of SWR messages to the log
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    LOOP AT swr ASSIGNING FIELD-SYMBOL(<swr>).
      add_t100_msg( msgid    = <swr>-msgid
                    msgno    = <swr>-msgno
                    msgty    = <swr>-msgty
                    msgv1    = <swr>-msgv1
                    msgv2    = <swr>-msgv2
                    msgv3    = <swr>-msgv3
                    msgv4    = <swr>-msgv4
                    cumulate = abap_false ).
    ENDLOOP.
  ENDMETHOD.


  METHOD add_swr_messag.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Adds a list of SWR messages to the log
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    LOOP AT swr ASSIGNING FIELD-SYMBOL(<swr>).
      add_free_text( text  = <swr>-line
                     msgty = <swr>-msg_type ).
    ENDLOOP.
  ENDMETHOD.


  METHOD add_sy_msg.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Adds a SY-MSG to the log
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    add_t100_msg( msgid    = sy-msgid
                  msgno    = sy-msgno
                  msgty    = sy-msgty
                  msgv1    = sy-msgv1
                  msgv2    = sy-msgv2
                  msgv3    = sy-msgv3
                  msgv4    = sy-msgv4
                  cumulate = cumulate ).
  ENDMETHOD.


  METHOD add_t100_msg.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Adds T100 messages to the log
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(log_msg) = VALUE bal_s_msg(
        msgty     = msgty
        msgid     = msgid
        msgno     = msgno
        probclass = determine_pclass( msgty ) ).

    WRITE: msgv1 TO log_msg-msgv1 LEFT-JUSTIFIED,
           msgv2 TO log_msg-msgv2 LEFT-JUSTIFIED,
           msgv3 TO log_msg-msgv3 LEFT-JUSTIFIED,
           msgv4 TO log_msg-msgv4 LEFT-JUSTIFIED.

    IF cumulate = abap_false.
      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle  = me->log_handle
          i_s_msg       = log_msg
        EXCEPTIONS
          log_not_found = 0
          OTHERS        = 1 ##FM_SUBRC_OK.

    ELSE.
      CALL FUNCTION 'BAL_LOG_MSG_CUMULATE'
        EXPORTING
          i_log_handle     = me->log_handle
          i_s_msg          = log_msg
        EXCEPTIONS
          log_not_found    = 1
          msg_inconsistent = 2
          log_is_full      = 3
          OTHERS           = 4 ##FM_SUBRC_OK.
    ENDIF.
  ENDMETHOD.


  METHOD clear_log.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Clears the messages in the memory
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    CHECK me->log_handle IS NOT INITIAL.

    DATA(msg_handle) = CONV balmsghndl( me->log_handle ).

    CALL FUNCTION 'BAL_LOG_MSG_DELETE'
      EXPORTING
        i_s_msg_handle = msg_handle
      EXCEPTIONS
        msg_not_found  = 1
        log_not_found  = 2
        OTHERS         = 3 ##FM_SUBRC_OK.

    CALL FUNCTION 'BAL_LOG_MSG_DELETE_ALL'
      EXPORTING
        i_log_handle  = log_handle
      EXCEPTIONS
        log_not_found = 1
        OTHERS        = 2 ##FM_SUBRC_OK.
  ENDMETHOD.


  METHOD constructor.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Class constructor
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    me->object    = object.
    me->subobject = subobject.

    DATA(log_param) = VALUE bal_s_log(
        object     = me->object
        subobject  = me->subobject
        aluser     = sy-uname
        altcode    = sy-tcode
        alprog     = sy-cprog
        almode     = COND #( WHEN sy-batch IS INITIAL THEN 'D' ELSE 'B' )
        aldate_del = COND #( WHEN preservation_days IS NOT INITIAL THEN sy-datum + preservation_days )
        extnumber  = extnumber ).

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log      = log_param
      IMPORTING
        e_log_handle = me->log_handle
      EXCEPTIONS
        OTHERS       = 1.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ycx_simbal_log
        EXPORTING
          textid    = ycx_simbal_log=>cant_create_instance
          object    = me->object
          subobject = me->subobject.
    ENDIF.
  ENDMETHOD.


  METHOD determine_pclass.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Determines problem class
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    output = COND #( WHEN msgty IN get_crit_msgty_range( )   THEN '1'
                     WHEN msgty  = ycl_simbal=>msgty-warning THEN '2'
                     WHEN msgty  = ycl_simbal=>msgty-info OR
                          msgty  = ycl_simbal=>msgty-status  THEN '4'
                     ELSE '2' ).
  ENDMETHOD.


  METHOD get_crit_msgty_range.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns a list of critical message types
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF ycl_simbal=>critical_message_type_rng IS INITIAL.
      ycl_simbal=>critical_message_type_rng = VALUE #(
          option = ycl_simbal=>option-eq
          sign   = ycl_simbal=>sign-include
          ( low = ycl_simbal=>msgty-exit )
          ( low = ycl_simbal=>msgty-abort )
          ( low = ycl_simbal=>msgty-error ) ).
    ENDIF.

    output = ycl_simbal=>critical_message_type_rng.
  ENDMETHOD.


  METHOD get_json_text.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Converts the text to an acceptable JSON string
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA text TYPE text100.
    WRITE val TO text LEFT-JUSTIFIED.
    output = text.

    REPLACE ALL OCCURRENCES OF:
        '\' IN output WITH '\\',
        '"' IN output WITH '\"'.
  ENDMETHOD.


  METHOD get_messages.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns messages in BAPIRET2 format
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA msg_handles TYPE bal_t_msgh.
    DATA msg         TYPE bal_s_msg.
    DATA msg_filter  TYPE bal_s_mfil.
    DATA msg_bapi    TYPE bapiret2.

    CHECK me->log_handle IS NOT INITIAL.

    IF msgty_x IS NOT INITIAL.
      APPEND VALUE #( option = me->option-eq
                      sign   = me->sign-include
                      low    = me->msgty-exit
                    ) TO msg_filter-msgty.
    ENDIF.

    IF msgty_a IS NOT INITIAL.
      APPEND VALUE #( option = me->option-eq
                      sign   = me->sign-include
                      low    = me->msgty-abort
                    ) TO msg_filter-msgty.
    ENDIF.

    IF msgty_e IS NOT INITIAL.
      APPEND VALUE #( option = me->option-eq
                      sign   = me->sign-include
                      low    = me->msgty-error
                    ) TO msg_filter-msgty.
    ENDIF.

    IF msgty_w IS NOT INITIAL.
      APPEND VALUE #( option = me->option-eq
                      sign   = me->sign-include
                      low    = me->msgty-warning
                    ) TO msg_filter-msgty.
    ENDIF.

    IF msgty_i IS NOT INITIAL.
      APPEND VALUE #( option = me->option-eq
                      sign   = me->sign-include
                      low    = me->msgty-info
                    ) TO msg_filter-msgty.
    ENDIF.

    IF msgty_s IS NOT INITIAL.
      APPEND VALUE #( option = me->option-eq
                      sign   = me->sign-include
                      low    = me->msgty-status
                    ) TO msg_filter-msgty.
    ENDIF.

    IF msg_filter-msgty IS INITIAL.
      RETURN.
    ENDIF.

    DATA(log_handles) = VALUE bal_t_logh( ( me->log_handle ) ).

    CALL FUNCTION 'BAL_GLB_SEARCH_MSG'
      EXPORTING
        i_t_log_handle = log_handles
        i_s_msg_filter = msg_filter
      IMPORTING
        e_t_msg_handle = msg_handles
      EXCEPTIONS
        msg_not_found  = 1.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT msg_handles ASSIGNING FIELD-SYMBOL(<msg_handle>).
      CALL FUNCTION 'BAL_LOG_MSG_READ'
        EXPORTING
          i_s_msg_handle = <msg_handle>
          i_langu        = sy-langu
        IMPORTING
          e_s_msg        = msg
        EXCEPTIONS
          log_not_found  = 1
          msg_not_found  = 2.

      CHECK sy-subrc = 0.

      CALL FUNCTION 'BALW_BAPIRETURN_GET2'
        EXPORTING
          type   = msg-msgty
          cl     = msg-msgid
          number = msg-msgno
          par1   = msg-msgv1
          par2   = msg-msgv2
          par3   = msg-msgv3
          par4   = msg-msgv4
        IMPORTING
          return = msg_bapi.

      APPEND msg_bapi TO output.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_messages_as_json.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns messages in JSON format
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(messages_as_itab) = get_messages(
               msgty_x  = msgty_x
               msgty_a  = msgty_a
               msgty_e  = msgty_e
               msgty_w  = msgty_w
               msgty_i  = msgty_i
               msgty_s  = msgty_s ).

    output = '['.

    LOOP AT messages_as_itab ASSIGNING FIELD-SYMBOL(<msg>).
      IF sy-tabix > 1.
        output = |{ output },|.
      ENDIF.

      IF short = abap_true.
        output = |{ output }| &&
                  | \{ | &&
                  |"type": "{ get_json_text( <msg>-type ) }", | &&
                  |"id": "{ get_json_text( <msg>-id ) }", | &&
                  |"number": "{ get_json_text( <msg>-number ) }", | &&
                  |"message": "{ get_json_text( <msg>-message ) }" | &&
                  | \}|.
      ELSE.
        output = |{ output }| &&
                  | \{ | &&
                  |"type": "{ get_json_text( <msg>-type ) }", | &&
                  |"id": "{ get_json_text( <msg>-id ) }", | &&
                  |"number": "{ get_json_text( <msg>-number ) }", | &&
                  |"message": "{ get_json_text( <msg>-message ) }", | &&
                  |"log_no": "{ get_json_text( <msg>-log_no ) }", | &&
                  |"log_msg_no": "{ get_json_text( <msg>-log_msg_no ) }", | &&
                  |"message_v1": "{ get_json_text( <msg>-message_v1 ) }", | &&
                  |"message_v2": "{ get_json_text( <msg>-message_v2 ) }", | &&
                  |"message_v3": "{ get_json_text( <msg>-message_v3 ) }", | &&
                  |"message_v4": "{ get_json_text( <msg>-message_v4 ) }", | &&
                  |"parameter": "{ get_json_text( <msg>-parameter ) }", | &&
                  |"row": "{ get_json_text( <msg>-row ) }", | &&
                  |"field": "{ get_json_text( <msg>-field ) }", | &&
                  |"system": "{ get_json_text( <msg>-system ) }" | &&
                  | \}|.
      ENDIF.
    ENDLOOP.

    output = |{ output }]|.
  ENDMETHOD.


  METHOD get_message_count.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns the number of messages
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    output = lines( get_messages(
        msgty_x = msgty_x
        msgty_a = msgty_a
        msgty_e = msgty_e
        msgty_w = msgty_w
        msgty_i = msgty_i
        msgty_s = msgty_s ) ).
  ENDMETHOD.


  METHOD get_text_of_msgid.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns the text of message ID
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA msg_bapi TYPE bapiret2.

    CALL FUNCTION 'BALW_BAPIRETURN_GET2'
      EXPORTING
        type   = me->msgty-status
        cl     = cl
        number = number
      IMPORTING
        return = msg_bapi.

    output = msg_bapi-message.
  ENDMETHOD.


  METHOD get_worst_severity.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Returns the worst severity among log messages
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(msg) = get_messages( ).

    LOOP AT msg TRANSPORTING NO FIELDS WHERE type IN get_crit_msgty_range(  ).
      output = me->severity-error.
      RETURN.
    ENDLOOP.

    IF line_exists( msg[ type = me->msgty-warning ] ).
      output = me->severity-warning.
      RETURN.
    ENDIF.

    output = me->severity-all.
  ENDMETHOD.


  METHOD save_to_db.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Saves log messages to the database
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(log_handles) = VALUE bal_t_logh( ( me->log_handle ) ).

    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_in_update_task = abap_true
        i_save_all       = save_all
        i_t_log_handle   = log_handles
      EXCEPTIONS
        log_not_found    = 1
        save_not_allowed = 2
        numbering_error  = 3
        OTHERS           = 4.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ycx_simbal_log
        EXPORTING
          textid    = ycx_simbal_log=>cant_save
          object    = me->object
          subobject = me->subobject.
    ENDIF.

    IF commit = abap_true.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDMETHOD.


  METHOD save_to_db_2nd_connection.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    " Saves log messages to the database using a second connection
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA(log_handles) = VALUE bal_t_logh( ( me->log_handle ) ).

    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_in_update_task     = abap_false
        i_save_all           = save_all
        i_t_log_handle       = log_handles
        i_2th_connection     = abap_true
        i_2th_connect_commit = abap_true
      EXCEPTIONS
        log_not_found        = 1
        save_not_allowed     = 2
        numbering_error      = 3
        OTHERS               = 4.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ycx_simbal_log
        EXPORTING
          textid    = ycx_simbal_log=>cant_save
          object    = me->object
          subobject = me->subobject.
    ENDIF.
  ENDMETHOD. "save_to_db
ENDCLASS.
