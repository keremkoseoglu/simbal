class YCX_SIMBAL_LOG_SAVE definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

  constants:
    begin of YCX_SIMBAL_LOG_SAVE,
      msgid type symsgid value 'YSIMBAL',
      msgno type symsgno value '003',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of YCX_SIMBAL_LOG_SAVE .
  constants:
    begin of CANT_SAVE,
      msgid type symsgid value 'YSIMBAL',
      msgno type symsgno value '004',
      attr1 type scx_attrname value 'OBJECT',
      attr2 type scx_attrname value 'SUBOBJECT',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of CANT_SAVE .
  data OBJECT type BALOBJ_D .
  data SUBOBJECT type BALSUBOBJ .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !OBJECT type BALOBJ_D optional
      !SUBOBJECT type BALSUBOBJ optional .
protected section.
private section.
ENDCLASS.



CLASS YCX_SIMBAL_LOG_SAVE IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->OBJECT = OBJECT .
me->SUBOBJECT = SUBOBJECT .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = YCX_SIMBAL_LOG_SAVE .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
